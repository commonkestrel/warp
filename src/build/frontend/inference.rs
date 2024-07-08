use crate::{build::syntax::ast::Mutability, diagnostic::Diagnostic, span::Spanned};

pub mod weak {
    pub mod hir;
    pub mod lib;
}

pub mod strong {
    pub mod mir;
}

pub fn resolve() -> Result<Namespace, Diagnostic> {
    todo!()
}

pub struct Namespace {}

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
