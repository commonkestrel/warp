use std::{
    fmt::Display,
    ops::{Deref, Range},
    sync::Arc,
};

use async_std::{fs::File, path::PathBuf};

use crate::{
    build::{
        lib::locator::locate_library,
        symbol_table::SymbolTable,
        syntax::{info::CompInfo, lex::Keyword},
    },
    debug,
    diagnostic::{Diagnostic, Reporter},
    info, seek,
    span::{Lookup, Span, Spanned},
    spanned_error, spanned_info,
};

use super::{
    ast::{Const, Function, Path, Progmem, Static},
    info::{Lib, LibSrc},
    lex::{lex, Delimeter, Punctuation, Token},
    token::{
        CloseBrace, CloseBracket, CloseParen, Gt, Ident, Lt, OpenBrace, OpenBracket, OpenParen,
    },
};

pub async fn parse(
    stream: &[Spanned<Token>],
    source_name: Arc<String>,
    lookup: Arc<Lookup>,
    reporter: Reporter,
    symbol_table: &SymbolTable,
    subdir: PathBuf,
) -> Result<Namespace, Reporter> {
    let mut cursor = Cursor::new(stream, source_name, lookup, symbol_table, reporter);
    Namespace::parse(&mut cursor, subdir)
        .await
        .map_err(|_| cursor.take_reporter())
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

    async fn parse<'a>(cursor: &mut Cursor<'a>, subdir: PathBuf) -> Result<Namespace, ()> {
        let mut namespace = Namespace::new(subdir.clone());
        let mut visibility = Visibility::Private;

        while let Some(tok) = cursor.peek() {
            match tok.inner() {
                Token::Keyword(Keyword::Fn) => match cursor.parse() {
                    Ok(func) => {
                        namespace.functions.push((func, visibility));
                        visibility = Visibility::Private;
                    }
                    Err(err) => {
                        cursor.reporter().report(err).await;
                        continue;
                    }
                },
                Token::Keyword(Keyword::Import) => {
                    if visibility != Visibility::Private {
                        cursor
                            .reporter()
                            .report(spanned_error!(tok.span().clone(), "imports cannot "))
                            .await;
                    }
                    cursor.step();

                    match cursor.parse() {
                        Ok(path) => namespace.imports.push(path),
                        Err(err) => {
                            cursor.reporter().report(err).await;
                            cursor.seek(&Token::Punctuation(Punctuation::Semicolon));
                        }
                    }

                    cursor.expect_semicolon();
                    visibility = Visibility::Private;
                }
                Token::Keyword(Keyword::Const) => {
                    match cursor.parse() {
                        Ok(var) => namespace.constants.push((var, visibility)),
                        Err(err) => {
                            cursor.reporter().report(err).await;
                            cursor.seek(&Token::Punctuation(Punctuation::Semicolon));
                        }
                    }

                    cursor.expect_semicolon();
                    visibility = Visibility::Private;
                }
                Token::Keyword(Keyword::Static) => {
                    match cursor.parse() {
                        Ok(var) => namespace.statics.push((var, visibility)),
                        Err(err) => {
                            cursor.reporter().report(err).await;
                            cursor.seek(&Token::Punctuation(Punctuation::Semicolon));
                        }
                    }

                    cursor.expect_semicolon();
                    visibility = Visibility::Private;
                }
                Token::Keyword(Keyword::Progmem) => {
                    match cursor.parse() {
                        Ok(var) => namespace.progmem.push((var, visibility)),
                        Err(err) => {
                            cursor.reporter().report(err).await;
                            cursor.seek(&Token::Punctuation(Punctuation::Semicolon));
                        }
                    }

                    cursor.expect_semicolon();
                    visibility = Visibility::Private;
                }
                Token::Keyword(Keyword::Prot) => {
                    match visibility {
                        Visibility::Public | Visibility::Protected => {
                            cursor
                                .reporter()
                                .report(spanned_error!(
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
                            cursor
                                .reporter()
                                .report(spanned_error!(
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
                    let ident = match cursor.parse::<Spanned<Ident>>() {
                        Ok(ident) => ident,
                        Err(err) => {
                            cursor.reporter().report(err).await;
                            seek!(
                                cursor,
                                Token::Punctuation(Punctuation::Semicolon)
                                    | Token::Delimeter(Delimeter::CloseBrace)
                            );
                            cursor.step();
                            continue;
                        }
                    };

                    match cursor.peek() {
                        Some(Spanned {
                            inner: Token::Delimeter(Delimeter::OpenBrace),
                            span,
                        }) => {
                            let start_span = span.clone();
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
                            cursor.expect_semicolon();

                            let subspace_path = subdir
                                .join(cursor.symbol_table.get(ident.inner().symbol).await)
                                .with_extension("warp");
                            let file_name = subspace_path.to_string_lossy().replace("\\", "/");
                            let subspace_file = match File::open(&subspace_path).await {
                                Ok(file) => file,
                                Err(err) => {
                                    cursor
                                        .reporter()
                                        .report(spanned_error!(
                                            ident.into_span(),
                                            "unable to open file `{file_name}`: {err}"
                                        ))
                                        .await;
                                    continue;
                                }
                            };

                            let lexed =
                                match lex(cursor.symbol_table.clone(), file_name, subspace_file)
                                    .await
                                {
                                    Ok(lexed) => lexed,
                                    Err(errors) => {
                                        cursor.reporter().report_all(errors).await;
                                        continue;
                                    }
                                };

                            let subspace_dir = subspace_path.with_extension("");
                            let mut cursor = Cursor::new(
                                &lexed.stream,
                                lexed.source,
                                lexed.lookup,
                                &lexed.symbol_table,
                                cursor.reporter().clone(),
                            );
                            if let Ok(subspace) =
                                Box::pin(Namespace::parse(&mut cursor, subspace_dir)).await
                            {
                                let eof = cursor.eof_span();
                                let end = eof.end();
                                let span = eof.with_location(0..end);

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
                        cursor
                            .reporter()
                            .report(spanned_error!(
                                tok.span().clone(),
                                "compiler info cannot contain visibility modifiers"
                            ))
                            .await;
                        cursor.step();
                        continue;
                    }

                    match CompInfo::parse(
                        Spanned::new(info.clone(), tok.span().clone()),
                        cursor.reporter(),
                    ) {
                        CompInfo::Lib(lib) => {
                            match locate_library(lib.src, cursor.reporter()).await {
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
                    cursor
                        .reporter()
                        .report(
                            spanned_error!(
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

        if cursor.reporter().has_errors() {
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
    pub position: usize,
    eof_span: Span,
    reporter: Reporter,
    symbol_table: &'a SymbolTable,
}

impl<'a> Cursor<'a> {
    #[inline]
    pub fn new(
        stream: &'a [Spanned<Token>],
        source_name: Arc<String>,
        lookup: Arc<Lookup>,
        symbol_table: &'a SymbolTable,
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
            symbol_table,
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
            self.symbol_table,
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
