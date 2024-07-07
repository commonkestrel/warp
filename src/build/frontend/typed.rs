use crate::span::Spanned;

#[derive(Debug, Clone)]
pub struct Database {

}

#[derive(Debug, Clone)]
pub struct Function {
    parameters: Vec<Parameter>,
    return_type: Spanned<Type>,
    body: Spanned<Statement>
}

#[derive(Debug, Clone)]
pub struct Parameter {

}

#[derive(Debug, Clone)]
pub enum Statement {

}

#[derive(Debug, Clone)]
pub enum Type {

}
