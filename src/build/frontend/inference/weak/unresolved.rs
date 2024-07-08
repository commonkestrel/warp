use std::{collections::HashMap, ops::Index, sync::Arc};

use async_std::path::PathBuf;
use slotmap::{new_key_type, SlotMap};

use crate::{
    build::{
        ascii::AsciiStr,
        frontend::{inference::Type, uncaught_error::UncaughtUnwrap},
        symbol_table::SymbolTable,
        syntax::{
            ast::{BinaryOp, Expr, Mutability, Path, PathSegment, Statement, UnaryOp},
            parse::{Namespace, Visibility},
            token::Ident,
        },
    },
    diagnostic::{Diagnostic, Reporter},
    span::{Span, Spanned},
    spanned_error,
};

use super::lib::resolve_lib;

pub struct UnresolvedDb {
    items: HashMap<Ident, Visible<Arc<Spanned<Item>>>>,
    imports: HashMap<PathSegment, Visible<Arc<Spanned<Path>>>>,
    libs: HashMap<Ident, Spanned<PathBuf>>,
}

impl UnresolvedDb {
    pub async fn compile(
        src: Namespace,
        symbol_table: SymbolTable,
        libraries: &mut HashMap<PathBuf, UnresolvedDb>,
        reporter: Reporter,
    ) -> UnresolvedDb {
        let mut items = HashMap::new();
        let mut imports = HashMap::new();
        let mut libs = HashMap::new();

        for (ident, path) in src.lib_imports {
            let lib_path = match path.canonicalize().await {
                Ok(path) => path,
                Err(err) => {
                    reporter
                        .report(spanned_error!(
                            ident.into_span(),
                            "unable to canonicalize path: {err}"
                        ))
                        .await;
                    continue;
                }
            };

            if !libraries.contains_key(&lib_path) {
                let lib = resolve_lib(
                    lib_path.clone(),
                    ident.span(),
                    libraries,
                    symbol_table.clone(),
                    &reporter,
                )
                .await;
                libraries.insert(lib_path.clone(), lib);
            }

            let (identifier, span) = ident.deconstruct();
            libs.insert(identifier, Spanned::new(lib_path, span));
        }

        for (spanned, vis) in src.functions {
            let (func, span) = spanned.deconstruct();
            let (ident, ident_span) = func.ident.deconstruct();

            let return_type = match func.return_type.unwrap() {
                Ok(ty) => ty,
                Err(err) => {
                    reporter.report(err).await;
                    continue;
                }
            };

            let mut parameters = Vec::new();

            for (param, span) in func.parameters.into_values().map(Spanned::deconstruct) {
                let ty = match param.ty.unwrap() {
                    Ok(ty) => ty,
                    Err(err) => {
                        reporter.report(err).await;
                        continue;
                    }
                };

                let parameter = Parameter {
                    mutability: param.mutability,
                    ident: param.ident,
                    ty,
                };

                parameters.push(Spanned::new(parameter, span));
            }

            let function = Arc::new(Spanned::new(
                Item::Fn(Function {
                    parameters,
                    return_type,
                    body: func.body,
                }),
                span,
            ));

            items.insert(ident, Visible::new(ident_span, vis, function));
        }

        for (path, vis) in src.imports {
            let (ident, ident_span) = path.end().clone().deconstruct();

            let import = Arc::new(path);

            imports.insert(ident, Visible::new(ident_span, vis, import));
        }

        for (spanned, vis) in src.constants {
            let (constant, const_span) = spanned.deconstruct();
            let (ident, ident_span) = constant.ident.deconstruct();

            let expr = Arc::new(Spanned::new(Item::Const(constant.value), const_span));
            if let Some(_) = items.insert(ident, Visible::new(ident_span.clone(), vis, expr)) {
                reporter.report(spanned_error!(ident_span, "duplicate identifier")).await;
            }
        }

        for (spanned, vis) in src.statics {
            let (stat, const_span) = spanned.deconstruct();
            let (ident, ident_span) = stat.ident.deconstruct();
            let ty = match stat.ty.unwrap() {
                Ok(ty) => ty,
                Err(err) => {
                    reporter.report(err).await;
                    continue;
                }
            };

            let expr = Arc::new(Spanned::new(Item::Static(Static {ty, value: stat.value}), const_span));
            if let Some(_) = items.insert(ident, Visible::new(ident_span.clone(), vis, expr)) {
                reporter.report(spanned_error!(ident_span, "duplicate identifier")).await;
            }
        }

        for (spanned, vis) in src.progmem {
            let (progmem, const_span) = spanned.deconstruct();
            let (ident, ident_span) = progmem.ident.deconstruct();
            let ty = match progmem.ty.unwrap() {
                Ok(ty) => ty,
                Err(err) => {
                    reporter.report(err).await;
                    continue;
                }
            };

            let expr = Arc::new(Spanned::new(Item::Progmem(Progmem {ty, value: progmem.value}), const_span));
            if let Some(_) = items.insert(ident, Visible::new(ident_span.clone(), vis, expr)) {
                reporter.report(spanned_error!(ident_span, "duplicate identifier")).await;
            }
        }

        UnresolvedDb { items, imports, libs }
    }

    pub fn get(&self, index: &Spanned<Ident>) -> Result<Visible<Arc<Spanned<Item>>>, Diagnostic> {
        match self.items.get(index.inner()) {
            Some(item) => Ok(item.clone()),
            None => Err(spanned_error!(
                index.span().clone(),
                "item does not exist in submodule or library"
            )),
        }
    }
}

impl Default for UnresolvedDb {
    fn default() -> Self {
        UnresolvedDb {
            items: HashMap::new(),
            imports: HashMap::new(),
            libs: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Visible<T> {
    ident_span: Span,
    visibility: Visibility,
    inner: T,
}

impl<T> Visible<T> {
    pub fn new(ident_span: Span, visibility: Visibility, inner: T) -> Self {
        Self {
            ident_span,
            visibility,
            inner,
        }
    }

    pub fn ident_span(&self) -> &Span {
        &self.ident_span
    }

    pub fn into_ident_span(self) -> Span {
        self.ident_span
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn into_inner(self) -> T {
        self.inner
    }
}

pub enum Item {
    Fn(Function),
    Const(Spanned<Expr>),
    Static(Static),
    Progmem(Progmem),
    Subspace(UnresolvedDb),
}

impl Item {
    fn description(&self) -> &'static str {
        match self {
            Item::Fn(_) => "function",
            Item::Const(_) => "constant",
            Item::Static(_) => "static",
            Item::Progmem(_) => "program memory",
            Item::Subspace(_) => "subspace",
        }
    }
}

impl Visible<Spanned<Item>> {
    pub fn get(
        &self,
        parent: Spanned<Ident>,
        idx: &Spanned<Ident>,
        reachable: Visibility,
    ) -> Result<Arc<Spanned<Item>>, Diagnostic> {
        match self.inner().inner() {
            Item::Subspace(db) => match db.get(idx) {
                Ok(item) => {
                    let visible = match item.visibility() {
                        Visibility::Private => matches!(reachable, Visibility::Private),
                        Visibility::Protected => {
                            matches!(reachable, Visibility::Private | Visibility::Protected)
                        }
                        Visibility::Public => matches!(
                            reachable,
                            Visibility::Private | Visibility::Protected | Visibility::Public
                        ),
                    };

                    if visible {
                        Ok(item.into_inner())
                    } else {
                        Err(spanned_error!(
                            idx.span().clone(),
                            "item is not visible to current space"
                        )
                        .with_note(format!("item is of {} visibility", item.visibility())))
                    }
                }
                Err(err) => Err(err),
            },
            _ => Err(spanned_error!(
                parent.into_span(),
                "{} is not a package or subspace",
                self.inner().description()
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Static {
    ty: Spanned<Type>,
    value: Spanned<Expr>,
}

#[derive(Debug, Clone)]
pub struct Progmem {
    ty: Spanned<Type>,
    value: Spanned<Expr>,
}

#[derive(Debug, Clone)]
pub struct Function {
    parameters: Vec<Spanned<Parameter>>,
    return_type: Spanned<Type>,
    body: Spanned<Statement>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    mutability: Mutability,
    ident: Spanned<Ident>,
    ty: Spanned<Type>,
}
