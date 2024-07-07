use std::{collections::HashMap, sync::Arc};

use async_std::path::PathBuf;
use slotmap::{new_key_type, SlotMap};

use crate::{build::{ascii::AsciiStr, frontend::inference::Type, symbol_table::SymbolTable, syntax::{ast::{BinaryOp, Mutability, Path, Statement, UnaryOp}, parse::{Namespace, Visibility}, token::Ident}}, diagnostic::Reporter, span::{Span, Spanned}, spanned_error};

use super::lib::resolve_lib;

pub struct Database {
    items: HashMap<Ident, Spanned<Visible<Item>>>,
    libs: HashMap<Ident, PathBuf>,
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
                let lib = resolve_lib(lib_path.clone(), ident.into_span(), libraries, symbol_table.clone(), &reporter).await;
                libraries.insert(lib_path, lib);
            }
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
    Const(Const),
    Static(Static),
    Progmem(Progmem),
    Subspace(Database),
}

#[derive(Debug, Clone)]
pub struct Const {}

#[derive(Debug, Clone)]
pub struct Static {}

#[derive(Debug, Clone)]
pub struct Progmem {}

#[derive(Debug, Clone)]
pub struct Function {
    parameters: Vec<Parameter>,
    return_type: Spanned<Type>,
    body: Spanned<Statement>
}

#[derive(Debug, Clone)]
pub struct Parameter {
    ident: Spanned<Ident>,
    ty: Spanned<Type>,
}
