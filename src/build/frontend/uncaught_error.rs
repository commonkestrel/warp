use crate::{build::syntax, diagnostic::Diagnostic, span::Spanned, spanned_error};

use super::inference;

pub trait UncaughtUnwrap {
    type Output;

    fn unwrap(self) -> Result<Self::Output, Diagnostic>;
}

impl UncaughtUnwrap for Spanned<syntax::ast::Type> {
    type Output = Spanned<inference::Type>;

    fn unwrap(self) -> Result<Self::Output, Diagnostic> {
        use syntax::ast::Type as ST;
        use inference::Type as IT;
        let (ty, span) = self.deconstruct();

        match ty {
            ST::Err => Err(spanned_error!(span, "uncaught STpe error").as_bug()),
            ST::Void => Ok(Spanned::new(IT::Void, span)),
            ST::Bool => Ok(Spanned::new(IT::Bool, span)),
            ST::U8 => Ok(Spanned::new(IT::U8, span)),
            ST::I8 => Ok(Spanned::new(IT::I8, span)),
            ST::U16 => Ok(Spanned::new(IT::U16, span)),
            ST::I16 => Ok(Spanned::new(IT::I16, span)),
            ST::U24 => Ok(Spanned::new(IT::U24, span)),
            ST::I24 => Ok(Spanned::new(IT::I24, span)),
            ST::U32 => Ok(Spanned::new(IT::U32, span)),
            ST::I32 => Ok(Spanned::new(IT::I32, span)),
            ST::U64 => Ok(Spanned::new(IT::U64, span)),
            ST::I64 => Ok(Spanned::new(IT::I64, span)),
            ST::Array(ty) => Ok(Spanned::new(IT::Array(Box::new(ty.unwrap()?)), span)),
            ST::Pointer {mutability, ty} => Ok(Spanned::new(IT::Pointer {mutability, ty: Box::new(ty.unwrap()?)}, span)),
            ST::Tuple(types) => {
                let mut unwrapped = Vec::new();

                for ty in types {
                    unwrapped.push(ty.unwrap()?);
                }

                Ok(Spanned::new(IT::Tuple(unwrapped), span))
            },
            ST::Fn {parameters, return_type} => {
                let mut unwrapped = Vec::new();

                for param in parameters.into_values() {
                    unwrapped.push(param.unwrap()?);
                }

                Ok(Spanned::new(IT::Fn{return_type: Box::new(return_type.unwrap()?), parameters: unwrapped}, span))
            }
        }
    }
}
