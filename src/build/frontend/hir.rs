use crate::build::syntax::{ast::Mutability, parse::Visibility};
use nurse::prelude::*;

pub mod weak {
    pub mod lib;
    pub mod unresolved;
}

pub mod strong {
    pub mod resolved;
}

#[derive(Debug, Clone)]
pub struct Database {}

#[derive(Debug, Clone)]
pub enum Type {
    U8,
    U16,
    U24,
    U32,
    U64,
    I8,
    I16,
    I24,
    I32,
    I64,
    Bool,
    Void,
    Pointer {
        mutability: Mutability,
        ty: Box<Spanned<Type>>,
    },
    Tuple(Vec<Spanned<Type>>),
    Array(Box<Spanned<Type>>),
    Fn {
        parameters: Vec<Spanned<Type>>,
        return_type: Box<Spanned<Type>>,
    },
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
