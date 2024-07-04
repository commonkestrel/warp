use crate::diagnostic::Diagnostic;

pub trait UncaughtUnwrap {
    type Output;

    fn unwrap(self) -> Result<Self::Output, Diagnostic>;
}
