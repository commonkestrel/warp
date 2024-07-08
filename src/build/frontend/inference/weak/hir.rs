use std::{collections::HashMap, ops::Index, sync::Arc};

use async_std::path::PathBuf;
use slotmap::{new_key_type, SlotMap};

use crate::{build::{ascii::AsciiStr, frontend::{inference::Type, uncaught_error::UncaughtUnwrap}, symbol_table::SymbolTable, syntax::{ast::{BinaryOp, Expr, Mutability, Path, Statement, UnaryOp}, parse::{Namespace, Visibility}, token::Ident}}, diagnostic::{Diagnostic, Reporter}, span::{Span, Spanned}, spanned_error};

use super::lib::resolve_lib;

pub struct Database {
    items: HashMap<Ident, Visible<Arc<Spanned<Item>>>>,
    libs: HashMap<Ident, Spanned<PathBuf>>,
}

impl Database {
    pub async fn resolve(src: Namespace, symbol_table: SymbolTable, libraries: &mut HashMap<PathBuf, Database>, reporter: Reporter) -> Database {
        let mut items = HashMap::new();
        let mut libs = HashMap::new();

        for (ident, path) in src.lib_imports {
            let lib_path = match path.canonicalize().await {
                Ok(path) => path,
                Err(err) => {
                    reporter.report(spanned_error!(ident.into_span(), "unable to canonicalize path: {err}")).await;
                    continue;
                }
            };

            if !libraries.contains_key(&lib_path) {
                let lib = resolve_lib(lib_path.clone(), ident.span(), libraries, symbol_table.clone(), &reporter).await;
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

            let function = Arc::new(Spanned::new(Item::Fn(Function {
                parameters,
                return_type,
                body: func.body,
            }), span));

            items.insert(ident, Visible::new(ident_span, vis, function));
        }

        for (spanned, vis) in src.imports {
            let (path, path_span) = spanned.deconstruct();
            
        }

        Database { items, libs }
    }
}

impl Default for Database {
    fn default() -> Self {
        Database {
            items: HashMap::new(),
            libs: HashMap::new(),
        }
    }
}

pub struct Visible<T> {
    ident_span: Span,
    visibility: Visibility,
    inner: T,
}

impl<T> Visible<T> {
    pub fn new(ident_span: Span, visibility: Visibility, inner: T) -> Self {
        Self {ident_span, visibility, inner}
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
    Const(Expr),
    Static(Static),
    Progmem(Progmem),
    Subspace(Database),
    Pkg(Database),
}

impl Item {
    fn description(&self) -> &'static str {
        match self {
            Item::Fn(_) => "function",
            Item::Const(_) => "constant",
            Item::Static(_) => "static",
            Item::Progmem(_) => "program memory",
            Item::Subspace(_) => "subspace",
            Item::Pkg(_) => "package"
        }
    }
}

impl Spanned<Item> {
    pub fn get(&self, reachable: Visibility) -> Result<Arc<Spanned<Item>>, Diagnostic> {
        match self.inner() {
            _ => Err(spanned_error!(self.span().clone(), "{} is not a package or subspace", self.description()))
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
    body: Spanned<Statement>
}

#[derive(Debug, Clone)]
pub struct Parameter {
    mutability: Mutability,
    ident: Spanned<Ident>,
    ty: Spanned<Type>,
}
