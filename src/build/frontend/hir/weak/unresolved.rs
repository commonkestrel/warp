use std::{collections::HashMap, ops::Index, sync::Arc};

use async_std::path::PathBuf;
use slotmap::{new_key_type, SlotMap};

use crate::{
    build::{
        ascii::AsciiStr,
        frontend::{hir::{Type, Visible}, uncaught_error::UncaughtUnwrap},
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

new_key_type! {pub struct ItemId;}

#[derive(Debug, Clone)]
pub struct UnresolvedDb {
    items: HashMap<Ident, Visible<ItemId>>,
    imports: HashMap<Ident, (Span, Spanned<Path>)>,
    libs: HashMap<Ident, Spanned<PathBuf>>,
}

impl UnresolvedDb {
    pub async fn compile(
        src: Namespace,
        symbol_table: SymbolTable,
        libraries: &mut HashMap<PathBuf, UnresolvedDb>,
        items: &mut SlotMap<ItemId, Item>,
        reporter: Reporter,
    ) -> UnresolvedDb {
        let mut db = UnresolvedDb::default();

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
                    items,
                    symbol_table.clone(),
                    &reporter,
                )
                .await;
                libraries.insert(lib_path.clone(), lib);
            }

            let (identifier, span) = ident.deconstruct();
            db.libs.insert(identifier, Spanned::new(lib_path, span));
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

            let function = Item::Fn(Function {
                parameters,
                return_type,
                body: func.body,
            });

            let id = items.insert(function);
            if db.items.insert(ident, Visible::new(ident_span.clone(), vis, id)).is_some() {
                reporter.report(spanned_error!(ident_span, "duplicate identifier")).await;
            }
        }

        for path in src.imports {
            let (ident, ident_span) = match path.end_segment() {
                Some(seg) => seg.deconstruct(),
                None => continue,
            };

            let import = path;

            db.imports.insert(ident, (ident_span, import));
        }

        for (spanned, vis) in src.constants {
            let (constant, const_span) = spanned.deconstruct();
            let (ident, ident_span) = constant.ident.deconstruct();

            let expr = Item::Const(constant.value);
            let id = items.insert(expr);
            if let Some(_) = db.items.insert(ident, Visible::new(ident_span.clone(), vis, id)) {
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

            let item = Item::Static(Static {ty, value: stat.value});
            let id = items.insert(item);
            if let Some(_) = db.items.insert(ident, Visible::new(ident_span.clone(), vis, id)) {
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

            let item = Item::Progmem(Progmem {ty, value: progmem.value});
            let id = items.insert(item);
            if let Some(_) = db.items.insert(ident, Visible::new(ident_span.clone(), vis, id)) {
                reporter.report(spanned_error!(ident_span, "duplicate identifier")).await;
            }
        }

        for (id, subspace, vis) in src.subspaces {
            let (ident, ident_span) = id.deconstruct();
            let (space, space_span) = subspace.deconstruct();

            let subspace_db = Box::pin(UnresolvedDb::compile(space, symbol_table.clone(), libraries, items, reporter.clone())).await;
            let item = Item::Subspace(subspace_db);
            let id = items.insert(item);
            if let Some(_) = db.items.insert(ident, Visible::new(ident_span.clone(), vis, id)) {
                reporter.report(spanned_error!(ident_span, "duplicate identifier")).await;
            }
        }

        db
    }

    pub async fn resolve_path(&self, path: Path, root: ItemId, superspace: Option<ItemId>, items: &SlotMap<ItemId, Item>, reporter: &Reporter) -> Option<ItemId> {
        let (start, mut base_span) = path.start().clone().deconstruct();
        let (mut item, same_package) = match start {
            PathSegment::Root => (root, true),
            PathSegment::Super => match superspace {
                Some(sup) => (sup, true),
                None => {
                    reporter.report(spanned_error!(base_span, "already at project root; no superspace available")).await;
                    return None;
                }
            },
            PathSegment::Ident(id) => match self.get_local(&Spanned::new(id, base_span.clone())) {
                Ok(item) => (item.0.into_inner(), item.1),
                Err(err) => {
                    reporter.report(err).await;
                    return None;
                }
            }
        };

        for segment in path.segments() {
            let base = match items.get(item) {
                Some(base) => base,
                None => {
                    reporter.report(spanned_error!(base_span, "no item found for referenced base").as_bug()).await;
                    return None;
                }
            };

            match todo!() {
                Ok(it) => item = it,
                Err(err) => {
                    reporter.report(err).await;
                    return None;
                }
            }
        }

        Some(item)
    }

    pub fn get_local(&self, index: &Spanned<Ident>) -> Result<(Spanned<ItemId>, bool), Diagnostic> {
        todo!()
    }

    pub fn get(&self, index: &Spanned<Ident>) -> Result<Visible<ItemId>, Diagnostic> {
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
pub enum Item {
    Fn(Function),
    Const(Spanned<Expr>),
    Static(Static),
    Progmem(Progmem),
    Import(ItemId),
    Subspace(UnresolvedDb),
    Library(UnresolvedDb),
}

impl Item {
    fn description(&self) -> &'static str {
        match self {
            Item::Fn(_) => "function",
            Item::Const(_) => "constant",
            Item::Static(_) => "static",
            Item::Import(_) => "import",
            Item::Progmem(_) => "program memory",
            Item::Subspace(_) => "subspace",
            Item::Library(_) => "library",
        }
    }

    pub fn get(
        &self,
        parent: &Spanned<Ident>,
        idx: &Spanned<Ident>,
        reachable: Visibility,
    ) -> Result<ItemId, Diagnostic> {
        match self {
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
                parent.span().clone(),
                "{} is not a package or subspace",
                self.description()
            )),
        }
    }
}

impl Visible<Spanned<Item>> {
    
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
