use crate::{build::ascii::AsciiStr, seek, span::Spanned, spanned_error, Token};

use super::{lex::{Delimeter, Keyword, Primitive, Punctuation, Token}, parse::{Parsable, Punctuated}, token::Ident};

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    Block(Vec<Spanned<Statement>>),
    If {
        condition: Spanned<Expr>,
        content: Box<Spanned<Statement>>,
        else_block: Option<Box<Spanned<Statement>>>,
    },
    For(Box<ForLoop>),
    While(Box<WhileLoop>),
    Break,
    Continue,
    Return(Spanned<Expr>),
    Var {
        mutability: Mutability,
        ident: Spanned<Ident>,
        ty: Spanned<Type>,
        assignment: Spanned<Expr>,
    }
}

impl Parsable for Spanned<Statement> {
    fn parse(cursor: &mut super::parse::Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        todo!()
    }

    fn description(&self) -> &'static str {
        "statement"
    }
}

#[derive(Debug, Clone)]
pub struct ForLoop {
    init: Spanned<Statement>,
    check: Spanned<Statement>,
    post: Spanned<Statement>,
    content: Spanned<Statement>,
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    check: Spanned<Statement>,
    contents: Box<Spanned<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Void,
    Immediate(i128),
    Boolean(bool),
    Str(AsciiStr),
    Reference(Path),
    Call(Box<Spanned<Expr>>, Punctuated<Spanned<Expr>, Token![,]>),
    Tuple(Vec<Spanned<Expr>>),
    Array(Punctuated<Spanned<Expr>, Token![,]>),
    BinaryOp(Box<BinOp>),
    UnaryOp(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    As(Box<Spanned<Expr>>, Spanned<Type>),
    Err,
}

impl Expr {

}

impl Parsable for Spanned<Expr> {
    fn parse(cursor: &mut super::parse::Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        todo!()
    }

    fn description(&self) -> &'static str {
        "expression"
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    
}

#[derive(Debug, Clone)]
pub struct BinOp {
    lhs: Expr,
    op: BinaryOp,
    rhs: Expr,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    AndEq,
    OrEq,
    XorEq,
    Equality,
    Inequality,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Index,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negative,
    Not,
    Deref,
    Ref(Mutability),
}

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
    Err,
}

impl From<&Primitive> for Type {
    fn from(value: &Primitive) -> Self {
        match value {
            Primitive::Void => Type::Void,
            Primitive::Bool => Type::Bool,
            Primitive::U8 => Type::U8,
            Primitive::U16 => Type::U16,
            Primitive::U24 => Type::U24,
            Primitive::U32 => Type::U32,
            Primitive::U64 => Type::U64,
            Primitive::I8 => Type::I8,
            Primitive::I16 => Type::I16,
            Primitive::I24 => Type::I24,
            Primitive::I32 => Type::I32,
            Primitive::I64 => Type::I64,
        }
    }
}

impl Parsable for Spanned<Type> {
    fn parse(cursor: &mut super::parse::Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let peek = cursor.peek();

        match peek.map(|spanned| spanned.inner()) {
            Some(Token::Primitive(p)) => Ok(Spanned::new(p.into(), peek.unwrap().span().clone())),
            Some(Token::Punctuation(Punctuation::Star)) => {
                let start = peek.unwrap().span().clone();
                cursor.step();

                let mutability = if cursor.check(&Token::Keyword(Keyword::Mut)) {
                    cursor.step();
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                };

                let ty: Spanned<Type> = cursor.parse()?;
                let span = start.to(ty.span());

                Ok(Spanned::new(Type::Pointer {
                    mutability,
                    ty: Box::new(ty),
                }, span))
            }
            Some(Token::Delimeter(Delimeter::OpenParen)) => {
                let start = peek.unwrap().span().clone();

                let mut comma = true;
                let mut types = Vec::new();

                while !cursor.check(&Token::Delimeter(Delimeter::CloseParen)) && !cursor.at_end() {
                    if !comma {
                        // We can unwrap here since we are already checking if the cursor has more tokens
                        let (next_tok, next_span) = cursor.next().unwrap().deconstruct();

                        cursor.reporter().report_sync(spanned_error!(next_span, "expected `,`, found {}", next_tok.description()));

                        seek!(cursor, Token::Delimeter(Delimeter::CloseParen) | Token::Punctuation(Punctuation::Comma));
                    } else {
                        types.push(cursor.parse()?);
                        comma = if cursor.check(&Token::Punctuation(Punctuation::Comma)) {
                            cursor.step();
                            true
                        } else {
                            false
                        };
                    }
                }

                let close: Spanned<Token![")"]> = match cursor.parse() {
                    Ok(close) => close,
                    Err(err) => {
                        cursor.reporter().report_sync(err);
                        return Ok(Spanned::new(Type::Err, cursor.eof_span()))
                    }
                };

                Ok(Spanned::new(
                    Type::Tuple(types),
                    start.to(close.span())
                ))
            }
            Some(tok) => {
                cursor.reporter().report_sync(spanned_error!(peek.unwrap().span().clone(), "expected type, found {}", tok.description()));
                Ok(Spanned::new(Type::Err, peek.unwrap().span().clone()))
            }
            None => {
                cursor.reporter().report_sync(spanned_error!(cursor.eof_span(), "expected type, found `EOF`"));
                Ok(Spanned::new(Type::Err, cursor.eof_span()))
            }
        }
    }

    fn description(&self) -> &'static str {
        "type"
    }
}

#[derive(Debug, Clone)]
pub enum Mutability {
    Immutable,
    Mutable,
}
