use std::collections::HashMap;

use slotmap::{new_key_type, SlotMap};

use crate::{build::{ascii::AsciiStr, frontend::inference::Type, syntax::{ast::{BinaryOp, Mutability, Path, Statement, UnaryOp}, parse::{Namespace, Visibility}, token::Ident}}, span::Spanned};

pub fn resolve(src: Namespace) -> Database {
    todo!()
}

pub type Database = HashMap<Ident, Spanned<Visible<Item>>>;

pub struct Visible<T> {
    visibility: Visibility,
    inner: T,
}

impl<T> Visible<T> {
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
