use std::{ops::{Range, Deref}, sync::Arc};

use crate::{
    diagnostic::{Diagnostic, Reporter},
    span::{Lookup, Span, Spanned},
    spanned_error,
};

use super::{ast::Function, info::LibSrc, lex::{Delimeter, Punctuation, Token}, token::{CloseBrace, CloseBracket, CloseParen, Gt, Lt, OpenBrace, OpenBracket, OpenParen}};

pub fn parse(stream: &[Spanned<Token>], source_name: Arc<String>, lookup: Arc<Lookup>, reporter: Reporter) -> Result<Namespace, Reporter> {
    let mut cursor = Cursor::new(stream, source_name, lookup, reporter);



    todo!()
}

pub struct Namespace {
    pub lib_imports: Vec<LibSrc>,
    pub functions: Vec<(Function, Visibility)>,

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Private,
    Protected,
    Public,
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct Punctuated<T, S> {
    inner: Vec<(T, S)>,
    last: Box<Option<T>>,
}

impl<T, S> Punctuated<T, S> {
    pub fn new(inner: Vec<(T, S)>, last: Option<T>) -> Self {
        Punctuated {
            inner,
            last: Box::new(last),
        }
    }

    pub fn empty() -> Self {
        Punctuated {
            inner: Vec::new(),
            last: Box::new(None),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len() + if self.last.is_some() { 1 } else { 0 }
    }

    pub fn last(&self) -> Option<&T> {
        (*self.last)
            .as_ref()
            .or_else(|| self.inner.last().map(|both| &both.0))
    }

    pub fn first(&self) -> Option<&T> {
        self.inner
            .get(0)
            .map(|both| &both.0)
            .or_else(|| (*self.last).as_ref())
    }

    pub fn values<'a>(&'a self) -> Box<dyn Iterator<Item = &T> + 'a> {
        Box::new(
            self.inner
                .iter()
                .map(|pair| &pair.0)
                .chain(self.last.iter()),
        )
    }
}

impl<T: Parsable, S: Parsable> Parsable for Punctuated<T, S> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let mut inner = Vec::new();
        let mut last = None;

        while !cursor.at_end() {
            let next = cursor.parse()?;
            if cursor.at_end() {
                last = Some(next);
                break;
            }
            let sep = cursor.parse()?;

            inner.push((next, sep));
        }

        Ok(Self {
            inner,
            last: Box::new(last),
        })
    }

    fn description(&self) -> &'static str {
        "punctuated expression"
    }
}

#[macro_export]
macro_rules! punctuated {
    ($cursor:expr, $content:pat, $seperator:pat$(,)?) => {{
        let mut inner = Vec::new();
        let mut last = None;

        while let Some(tok) = $cursor.peek() {
            match tok.inner() {
                $content => last = Some($cursor.parse()?),
                $seperator => match last.take() {
                    Some(l) => inner.push((l, $cursor.parse()?)),
                    None => {
                        return Err($crate::spanned_error!(
                            tok.span().clone(),
                            "unexpected duplicate seperator"
                        ))
                    }
                },
                _ => break,
            }
        }

        Ok(Punctuated::new(inner, last))
    }};
    ($cursor:expr, !$end:pat, $seperator:pat) => {{
        let mut inner = Vec::new();
        let mut last = None;
        let mut err = None;

        while let Some(tok) = $cursor.peek() {
            match tok.inner() {
                $end => break,
                $seperator => match last.take() {
                    Some(l) => inner.push((l, $cursor.parse()?)),
                    None => {
                        err = Some(tok.span().clone());
                        break;
                    }
                },
                _ => last = Some($cursor.parse()?),
            }
        }

        if let Some(span) = err {
            Err($crate::spanned_error!(
                span,
                "unexpected duplicate seperator"
            ))
        } else {
            Ok(Punctuated::new(inner, last))
        }
    }};
}

macro_rules! delimeterized {
    ($name:literal, $struct:ident, $fn:ident, $open:ident, $close:ident, $open_inner:pat, $close_inner:pat, $error:literal) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $struct<T> {
            open: $open,
            inner: T,
            close: $close,
        }

        impl<T> $struct<T> {
            pub fn inner(&self) -> &T {
                &self.inner
            }

            pub fn into_inner(self) -> T {
                self.inner
            }
        }

        impl<T> Deref for $struct<T> {
            type Target = T;

            fn deref(&self) -> &T {
                self.inner()
            }
        }

        impl<T: Parsable> Parsable for Spanned<$struct<T>> {
            fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                let open: Spanned<$open> = cursor.parse()?;

                let mut depth = 0;
                let start = cursor.position;

                for (i, tok) in (&cursor.stream[start..]).into_iter().enumerate() {
                    cursor.position = start + i;
                    match tok.inner() {
                        $open_inner => depth += 1,
                        $close_inner => {
                            if depth == 0 {
                                let close: Spanned<$close> = cursor.parse()?;
                                let span = open.span().to(close.span());
                                return Ok(Spanned::new(
                                    $struct {
                                        open: open.into_inner(),
                                        inner: T::parse(&mut cursor.slice(start..i))?,
                                        close: close.into_inner(),
                                    },
                                    span,
                                ));
                            }

                            depth -= 1;
                        }
                        _ => {}
                    }
                }

                Err(spanned_error!(
                    open.into_span(),
                    concat!("unmatched opening ", $name)
                ))
            }

            fn description(&self) -> &'static str {
                concat!($name, " expression")
            }
        }

        impl<T: Parsable> Parsable for $struct<T> {
            fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                let open: Spanned<$open> = cursor.parse()?;

                let mut depth = 0;
                let start = cursor.position;

                for (i, tok) in (&cursor.stream[start..]).into_iter().enumerate() {
                    cursor.position = start + i;
                    match tok.inner() {
                        $open_inner => depth += 1,
                        $close_inner => {
                            if depth == 0 {
                                let close: Spanned<$close> = cursor.parse()?;
                                return Ok($struct {
                                    open: open.into_inner(),
                                    inner: T::parse(&mut cursor.slice(start..(start + i)))?,
                                    close: close.into_inner(),
                                });
                            }

                            depth -= 1;
                        }
                        _ => {}
                    }
                }

                Err(spanned_error!(
                    open.into_span(),
                    concat!("unmatched opening ", $name)
                ))
            }

            fn description(&self) -> &'static str {
                concat!($name, " expression")
            }
        }

        pub fn $fn<'a>(cursor: &'a mut Cursor) -> Result<$struct<Cursor<'a>>, Diagnostic> {
            let open: Spanned<$open> = cursor.parse()?;
            let start = cursor.position;
            let mut depth = 0;

            while !cursor.at_end() {
                match cursor.peek().map(Spanned::inner) {
                    Some($open_inner) => depth += 1,
                    Some($close_inner) => {
                        if depth == 0 {
                            let close: Spanned<$close> = cursor.parse()?;
                            return Ok($struct {
                                open: open.into_inner(),
                                inner: cursor.slice(start..cursor.position),
                                close: close.into_inner(),
                            });
                        }
                        depth -= 1;
                    }
                    _ => {}
                }
                cursor.position += 1;
            }

            Err(spanned_error!(
                open.into_span(),
                concat!("unmatched opening ", $error)
            ))
        }
    };
}

delimeterized!(
    "parenthesized",
    Parenthesized,
    parenthesized,
    OpenParen,
    CloseParen,
    Token::Delimeter(Delimeter::OpenParen),
    Token::Delimeter(Delimeter::CloseParen),
    "parenthesis"
);
delimeterized!(
    "bracketed",
    Bracketed,
    bracketed,
    OpenBracket,
    CloseBracket,
    Token::Delimeter(Delimeter::OpenBracket),
    Token::Delimeter(Delimeter::CloseBracket),
    "bracket"
);
delimeterized!(
    "braced",
    Braced,
    braced,
    OpenBrace,
    CloseBrace,
    Token::Delimeter(Delimeter::OpenBrace),
    Token::Delimeter(Delimeter::CloseBrace),
    "brace"
);
delimeterized!(
    "arrowed",
    Arrowed,
    arrowed,
    Lt,
    Gt,
    Token::Punctuation(Punctuation::Lt),
    Token::Punctuation(Punctuation::Gt),
    "arrow"
);
