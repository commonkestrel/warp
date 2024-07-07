use crate::{build::{ascii::AsciiStr, frontend::inference::Type, syntax::{ast::{BinaryOp, Mutability, Path, Statement, UnaryOp}, token::Ident}}, span::Spanned};

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
