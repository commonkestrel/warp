use std::{ops::Range, sync::Arc};

use crate::{
    diagnostic::{Diagnostic, Reporter},
    span::{Lookup, Span, Spanned},
    spanned_error,
};

use super::lex::{Punctuation, Token};

pub struct Cursor<'a> {
    stream: &'a [Spanned<Token>],
    pub position: usize,
    eof_span: Span,
    reporter: Reporter,
}

impl<'a> Cursor<'a> {
    #[inline]
    pub fn new(
        stream: &'a [Spanned<Token>],
        source_name: Arc<String>,
        lookup: Arc<Lookup>,
        reporter: Reporter,
    ) -> Self {
        let end_position = stream
            .last()
            .map(|last| last.span().end()..(last.span().end() + 1))
            .unwrap_or(0..1);
        Self {
            stream,
            position: 0,
            eof_span: Span::new(source_name, lookup, end_position),
            reporter,
        }
    }

    #[inline]
    pub fn parse<T: Parsable>(&mut self) -> Result<T, Diagnostic> {
        T::parse(self)
    }

    #[inline]
    pub fn stream(&self) -> &[Spanned<Token>] {
        self.stream
    }

    #[inline]
    pub fn at_end(&self) -> bool {
        self.position >= self.stream.len()
    }

    #[inline]
    pub fn peek(&self) -> Option<&Spanned<Token>> {
        self.stream.get(self.position)
    }

    #[inline]
    pub fn peek2(&self) -> Option<&Spanned<Token>> {
        self.stream.get(self.position + 1)
    }

    pub fn check(&self, other: &Token) -> bool {
        self.stream
            .get(self.position)
            .map(|next| next.inner() == other)
            .unwrap_or(false)
    }

    pub fn check2(&self, other: &Token) -> bool {
        self.stream
            .get(self.position + 1)
            .map(|next| next.inner() == other)
            .unwrap_or(false)
    }

    #[inline]
    pub fn peek_offset(&self, offset: usize) -> Option<&Spanned<Token>> {
        self.stream.get(self.position + offset)
    }

    #[inline]
    pub fn step(&mut self) {
        self.position += 1;
    }

    #[inline]
    pub fn step_back(&mut self) {
        if self.position != 0 {
            self.position -= 1;
        }
    }

    pub fn seek(&mut self, target: &Token) {
        while !self.check(target) && self.position <= self.stream.len() {
            self.position += 1;
        }
    }

    #[track_caller]
    pub fn slice<R: Into<Range<usize>>>(&mut self, range: R) -> Cursor {
        Cursor::new(
            &self.stream[range.into()],
            self.eof_span.source_name(),
            self.eof_span.lookup(),
            self.reporter.clone(),
        )
    }

    #[inline]
    pub fn eof_span(&self) -> Span {
        self.eof_span.clone()
    }

    #[inline]
    pub fn reporter<'r>(&'r self) -> &'r Reporter {
        &self.reporter
    }

    #[inline]
    pub fn take_reporter(&self) -> Reporter {
        self.reporter.clone()
    }

    pub fn expect_semicolon(&mut self) {
        if self.check(&Token::Punctuation(Punctuation::Semicolon)) {
            self.step();
        } else {
            let next_span = match self.peek() {
                Some(tok) => tok.span().clone(),
                None => self.eof_span(),
            };
            self.reporter
                .report_sync(spanned_error!(next_span, "expected `;`"));
        }
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.stream.get(self.position);
        self.position += 1;
        ret.cloned()
    }
}

#[macro_export]
macro_rules! seek {
    ($cursor:expr, $token:pat) => {
        while !$cursor.at_end() {
            if matches!($cursor.peek().map(Spanned::inner), Some($token)) {
                break;
            }
            $cursor.position += 1;
        }
    };
}

pub trait Parsable: Sized {
    /// Parses the item from a stream of raw tokens.
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic>;
    /// Static description of the item.
    /// Used for error messages.
    fn description(&self) -> &'static str;
}
