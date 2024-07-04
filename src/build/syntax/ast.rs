use crate::{span::Spanned, spanned_error};

use super::{lex::{Delimeter, Primitive, Token}, parse::Parsable};

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
        ty: Box<Type>,
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

impl Parsable for Type {
    fn parse(cursor: &mut super::parse::Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let peek = cursor.peek();

        match peek.map(|spanned| spanned.inner()) {
            Some(Token::Primitive(p)) => Ok(p.into()),
            Some(Token::Delimeter(Delimeter::OpenParen)) => {
                todo!()
            }
            Some(tok) => {
                cursor.reporter().report_sync(spanned_error!(peek.unwrap().span().clone(), "expected type, found {}", tok.description()));
                Ok(Type::Err)
            }
            None => {
                cursor.reporter().report_sync(spanned_error!(cursor.eof_span(), "expected type, found `EOF`"));
                Ok(Type::Err)
            }
        }
    }

    fn description(&self) -> &'static str {
        "type"
    }
}

pub enum Mutability {
    Immutable,
    Mutable,
}
