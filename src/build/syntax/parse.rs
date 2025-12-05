use std::{
    fmt::Display,
    ops::{Deref, Range},
    path::PathBuf,
    io::Stdout,
};

use smol::fs::File;
use nurse::prelude::*;

use crate::{
    seek,
    build::{
        lib::locator::locate_library,
        symbol_table::SymbolTable,
        syntax::{info::CompInfo, lex::Keyword},
    }
};

use super::{
    ast::{Const, Function, Path, Progmem, Static},
    info::{Lib, LibSrc},
    lex::{lex, Delimeter, Punctuation, Token},
    token::{
        CloseBrace, CloseBracket, CloseParen, Gt, Ident, Lt, OpenBrace, OpenBracket, OpenParen,
    },
};

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

pub async fn parse(
    stream: &[Spanned<Token>],
    reporter: &TerminalReporter<Stdout>,
    symbol_table: &SymbolTable,
    subdir: PathBuf,
) -> Result<Namespace, ()> {
    if stream.is_empty() {
        Ok(Namespace::new(subdir))
    } else {
        let eof = reporter.eof_span(stream[0].span().lookup()).await;
        let mut cursor = Cursor::new(stream, symbol_table, eof);
        Namespace::parse(&mut cursor, reporter, subdir)
            .await
    }
    
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub subdir: PathBuf,
    pub lib_imports: Vec<(Spanned<Ident>, PathBuf)>,
    pub functions: Vec<(Spanned<Function>, Visibility)>,
    pub imports: Vec<Spanned<Path>>,
    pub constants: Vec<(Spanned<Const>, Visibility)>,
    pub statics: Vec<(Spanned<Static>, Visibility)>,
    pub progmem: Vec<(Spanned<Progmem>, Visibility)>,
    pub subspaces: Vec<(Spanned<Ident>, Spanned<Namespace>, Visibility)>,
}

impl Namespace {
    fn new(subdir: PathBuf) -> Namespace {
        Namespace {
            subdir,
            lib_imports: Vec::new(),
            functions: Vec::new(),
            imports: Vec::new(),
            constants: Vec::new(),
            statics: Vec::new(),
            progmem: Vec::new(),
            subspaces: Vec::new(),
        }
    }

    async fn parse<'a>(cursor: &mut Cursor<'a>, reporter: &TerminalReporter<Stdout>, subdir: PathBuf) -> Result<Namespace, ()> {
        let mut namespace = Namespace::new(subdir.clone());
        let mut visibility = Visibility::Private;

        while let Some(tok) = cursor.peek() {
            match tok.inner() {
                Token::Keyword(Keyword::Fn) => match cursor.parse(reporter).await {
                    Ok(func) => {
                        namespace.functions.push((func, visibility));
                        visibility = Visibility::Private;
                    }
                    Err(_) => continue,
                },
                Token::Keyword(Keyword::Import) => {
                    if visibility != Visibility::Private {
                        reporter
                            .report(error!(tok.span().clone(), "imports cannot "))
                            .await;
                    }
                    cursor.step();

                    match cursor.parse(reporter).await {
                        Ok(path) => namespace.imports.push(path),
                        Err(_) => cursor.seek(&Token::Punctuation(Punctuation::Semicolon)),
                    }

                    cursor.expect_semicolon(reporter).await;
                    visibility = Visibility::Private;
                }
                Token::Keyword(Keyword::Const) => {
                    match cursor.parse(reporter).await {
                        Ok(var) => namespace.constants.push((var, visibility)),
                        Err(_) => cursor.seek(&Token::Punctuation(Punctuation::Semicolon))
                    }

                    cursor.expect_semicolon(reporter).await;
                    visibility = Visibility::Private;
                }
                Token::Keyword(Keyword::Static) => {
                    match cursor.parse(reporter).await {
                        Ok(var) => namespace.statics.push((var, visibility)),
                        Err(err) => cursor.seek(&Token::Punctuation(Punctuation::Semicolon))
                    }

                    cursor.expect_semicolon(reporter).await;
                    visibility = Visibility::Private;
                }
                Token::Keyword(Keyword::Progmem) => {
                    match cursor.parse(reporter).await {
                        Ok(var) => namespace.progmem.push((var, visibility)),
                        Err(_) => cursor.seek(&Token::Punctuation(Punctuation::Semicolon))
                    }

                    cursor.expect_semicolon(reporter).await;
                    visibility = Visibility::Private;
                }
                Token::Keyword(Keyword::Prot) => {
                    match visibility {
                        Visibility::Public | Visibility::Protected => {
                            reporter
                                .report(error!(
                                    tok.span().clone(),
                                    "duplicate visibility modifier"
                                ))
                                .await
                        }
                        Visibility::Private => visibility = Visibility::Protected,
                    }

                    cursor.step();
                }
                Token::Keyword(Keyword::Pub) => {
                    match visibility {
                        Visibility::Public | Visibility::Protected => {
                            reporter
                                .report(error!(
                                    tok.span().clone(),
                                    "duplicate visibility modifier"
                                ))
                                .await
                        }
                        Visibility::Private => visibility = Visibility::Public,
                    }

                    cursor.step();
                }
                Token::Keyword(Keyword::Subspace) => {
                    cursor.step();
                    let ident: Spanned<Ident> = match cursor.parse(reporter).await {
                        Ok(ident) => ident,
                        Err(_) => {
                            seek!(
                                cursor,
                                Token::Punctuation(Punctuation::Semicolon)
                                    | Token::Delimeter(Delimeter::CloseBrace)
                            );
                            cursor.step();
                            continue;
                        }
                    };

                    let peek = cursor.peek();
                    match peek.map(Spanned::inner) {
                        Some(Token::Delimeter(Delimeter::OpenBrace)) => {
                            let start_span = peek.unwrap().span();
                            let mut depth = 0;
                            let start = cursor.position + 1;

                            while let Some(tok) = cursor.peek() {
                                match tok.inner() {
                                    Token::Delimeter(Delimeter::OpenBrace) => depth += 1,
                                    Token::Delimeter(Delimeter::CloseBrace) => {
                                        if depth == 1 {
                                            let space_span = start_span.to(tok.span());

                                            let space_subdir = subdir.join(
                                                cursor.symbol_table.get(ident.inner().symbol).await,
                                            );
                                            let mut space_cursor =
                                                cursor.slice(start..cursor.position);

                                            if let Ok(subspace) = Box::pin(Namespace::parse(
                                                &mut space_cursor,
                                                reporter,
                                                space_subdir,
                                            ))
                                            .await
                                            {
                                                namespace.subspaces.push((
                                                    ident,
                                                    Spanned::new(subspace, space_span),
                                                    visibility,
                                                ));
                                            }
                                            visibility = Visibility::Private;
                                            cursor.step();
                                            break;
                                        } else {
                                            depth -= 1;
                                        }
                                    }
                                    _ => {}
                                }

                                cursor.step()
                            }
                        }
                        _ => {
                            cursor.expect_semicolon(reporter).await;

                            let subspace_path = subdir
                                .join(cursor.symbol_table.get(ident.inner().symbol).await)
                                .with_extension("warp");
                            let file_name = subspace_path.to_string_lossy().replace("\\", "/");
                            let subspace_file = match File::open(&subspace_path).await {
                                Ok(file) => file,
                                Err(err) => {
                                    reporter
                                        .report(error!(
                                            ident,
                                            "unable to open file `{file_name}`: {err}"
                                        ))
                                        .await;
                                    continue;
                                }
                            };

                            let lexed =
                                match lex(cursor.symbol_table.clone(), file_name, subspace_file, reporter)
                                    .await
                                {
                                    Ok(lexed) => lexed,
                                    Err(_) => continue
                                };

                            let subspace_dir = subspace_path.with_extension("");
                            let mut cursor = Cursor::new(
                                &lexed.stream,
                                &lexed.symbol_table,
                                reporter.eof_span(lexed.lookup).await
                            );

                            if let Ok(subspace) =
                                Box::pin(Namespace::parse(&mut cursor, reporter, subspace_dir)).await
                            {
                                let eof = cursor.eof();
                                let span = Span::new(eof.lookup(), 0..eof.end());

                                namespace.subspaces.push((
                                    ident,
                                    Spanned::new(subspace, span),
                                    visibility,
                                ));
                            }
                            visibility = Visibility::Private;
                        }
                    }
                }
                Token::CompInfo(info) => {
                    if matches!(visibility, Visibility::Public | Visibility::Protected) {
                        reporter
                            .report(error!(
                                tok.span().clone(),
                                "compiler info cannot contain visibility modifiers"
                            ))
                            .await;
                        cursor.step();
                        continue;
                    }

                    match CompInfo::parse(
                        Spanned::new(info.clone(), tok.span().clone()),
                        reporter,
                    ).await {
                        CompInfo::Lib(lib) => {
                            match locate_library(lib.src, reporter).await {
                                Ok(path) => namespace.lib_imports.push((
                                    Spanned::new(
                                        Ident {
                                            symbol: cursor
                                                .symbol_table
                                                .find_or_insert(&lib.ident)
                                                .await,
                                        },
                                        lib.ident.span().clone(),
                                    ),
                                    path,
                                )),
                                Err(err) => {}
                            }
                        }
                        CompInfo::Err => {}
                    }
                    cursor.step()
                }
                _ => {
                    reporter
                        .report(
                            error!(
                                tok.span().clone(),
                                "unexpected token {}",
                                tok.description()
                            )
                            .with_note("instructions are not allowed in the top-level section"),
                        )
                        .await;
                    cursor.step();
                }
            }
        }

        if reporter.has_errors().await {
            Err(())
        } else {
            Ok(namespace)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Private,
    Protected,
    Public,
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Visibility::Private => "private",
                Visibility::Protected => "protected",
                Visibility::Public => "public",
            }
        )
    }
}

pub struct Cursor<'a> {
    stream: &'a [Spanned<Token>],
    symbol_table: &'a SymbolTable,
    pub position: usize,
    eof: Span,
}

impl<'a> Cursor<'a> {
    #[inline]
    pub fn new(
        stream: &'a [Spanned<Token>],
        symbol_table: &'a SymbolTable,
        eof: Span,
    ) -> Self {
        Self {
            stream,
            symbol_table,
            position: 0,
            eof,
        }
    }

    #[inline]
    pub async fn parse<T: Parsable>(&mut self, reporter: &TerminalReporter<Stdout>) -> Result<T, ()> {
        T::parse(self, reporter).await
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
            self.symbol_table,
            self.eof,
        )
    }

    #[inline]
    pub fn eof(&self) -> Span {
        self.eof
    }

    pub async fn expect_semicolon(&mut self, reporter: &TerminalReporter<Stdout>) {
        if self.check(&Token::Punctuation(Punctuation::Semicolon)) {
            self.step();
        } else {
            let next_span = match self.peek() {
                Some(tok) => tok.span(),
                None => self.eof,
            };
            reporter.report(error!(next_span, "expected `;`")).await;
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

pub trait Parsable: Sized {
    /// Parses the item from a stream of raw tokens.
    async fn parse(cursor: &mut Cursor, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()>;
    /// Static description of the item.
    /// Used for error messages.
    fn description(&self) -> &'static str;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Punctuated<T, S> {
    inner: Vec<(T, S)>,
    last: Box<Option<T>>,
}

impl<T: 'static, S: 'static> Punctuated<T, S> {
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

    pub fn into_values(self) -> Box<dyn Iterator<Item = T>> {
        Box::new(
            self.inner
                .into_iter()
                .map(|pair| pair.0)
                .chain(self.last.into_iter()),
        )
    }

    pub fn to_vec(self) -> Vec<T> {
        self.inner
            .into_iter()
            .map(|item| item.0)
            .chain(self.last.into_iter())
            .collect()
    }
}

impl<T: Parsable, S: Parsable> Parsable for Punctuated<T, S> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let mut inner = Vec::new();
        let mut last = None;

        while !cursor.at_end() {
            let next = cursor.parse(reporter).await?;
            if cursor.at_end() {
                last = Some(next);
                break;
            }
            let sep = cursor.parse(reporter).await?;

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
                $content => last = Some($cursor.parse().await?),
                $seperator => match last.take() {
                    Some(l) => inner.push((l, $cursor.parse().await?)),
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
                    Some(l) => inner.push((l, $cursor.parse().await?)),
                    None => {
                        err = Some(tok.span().clone());
                        break;
                    }
                },
                _ => last = Some($cursor.parse().await?),
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
            async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
                let open: Spanned<$open> = cursor.parse(reporter).await?;

                let mut depth = 0;
                let start = cursor.position;

                for (i, tok) in (&cursor.stream[start..]).into_iter().enumerate() {
                    cursor.position = start + i;
                    match tok.inner() {
                        $open_inner => depth += 1,
                        $close_inner => {
                            if depth == 0 {
                                let close: Spanned<$close> = cursor.parse(reporter).await?;
                                let span = open.span().to(close.span());
                                return Ok(Spanned::new(
                                    $struct {
                                        open: open.into_inner(),
                                        inner: T::parse(&mut cursor.slice(start..i), reporter).await?,
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

                reporter.report(error!(
                    open,
                    "unmatched opening {}",
                    $name
                )).await;
                Err(())
            }

            fn description(&self) -> &'static str {
                concat!($name, " expression")
            }
        }

        impl<T: Parsable> Parsable for $struct<T> {
            async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
                let open: Spanned<$open> = cursor.parse(reporter).await?;

                let mut depth = 0;
                let start = cursor.position;

                for (i, tok) in (&cursor.stream[start..]).into_iter().enumerate() {
                    cursor.position = start + i;
                    match tok.inner() {
                        $open_inner => depth += 1,
                        $close_inner => {
                            if depth == 0 {
                                let close: Spanned<$close> = cursor.parse(reporter).await?;
                                return Ok($struct {
                                    open: open.into_inner(),
                                    inner: T::parse(&mut cursor.slice(start..(start + i)), reporter).await?,
                                    close: close.into_inner(),
                                });
                            }

                            depth -= 1;
                        }
                        _ => {}
                    }
                }

                reporter.report(error!(
                    open,
                    "unmatched opening {}",
                    $name,
                )).await;
                Err(())
            }

            fn description(&self) -> &'static str {
                concat!($name, " expression")
            }
        }

        pub async fn $fn<'a>(cursor: &'a mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<$struct<Cursor<'a>>, ()> {
            let open: Spanned<$open> = cursor.parse(reporter).await?;
            let start = cursor.position;
            let mut depth = 0;

            while !cursor.at_end() {
                match cursor.peek().map(Spanned::inner) {
                    Some($open_inner) => depth += 1,
                    Some($close_inner) => {
                        if depth == 0 {
                            let close: Spanned<$close> = cursor.parse(reporter).await?;
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

            reporter.report(error!(
                open,
                "unmatched opening {}", 
                $error
            )).await;
            Err(())
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
