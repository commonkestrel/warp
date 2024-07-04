use std::{
    fmt::{Debug, Formatter},
    borrow,
    ops::{self, Range},
    sync::Arc,
};

use colored::{Color, Colorize};

#[derive(PartialEq, Clone)]
pub struct Spanned<T> {
    inner: T,
    span: Span,
}

impl<T> Spanned<T> {
    #[inline]
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned { inner: value, span }
    }

    #[inline]
    pub fn inner(&self) -> &T {
        &self.inner
    }

    #[inline]
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.inner
    }

    #[inline]
    pub fn span(&self) -> &Span {
        &self.span
    }

    #[inline]
    pub fn into_span(self) -> Span {
        self.span
    }

    #[inline]
    pub fn deconstruct(self) -> (T, Span) {
        (self.inner, self.span)
    }

    pub fn map<M, O>(self, map: M) -> Spanned<O>
    where
        M: FnOnce(T) -> O,
    {
        Spanned::new(map(self.inner), self.span)
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.inner())
    }
}

impl<T> ops::Deref for Spanned<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T> ops::DerefMut for Spanned<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner_mut()
    }
}

impl<T> borrow::Borrow<T> for Spanned<T> {
    #[inline]
    fn borrow(&self) -> &T {
        self.inner()
    }
}

impl<T> borrow::BorrowMut<T> for Spanned<T> {
    #[inline]
    fn borrow_mut(&mut self) -> &mut T {
        self.inner_mut()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    source_name: Arc<String>,
    lookup: Arc<Lookup>,
    location: Range<usize>,
}

impl Span {
    pub fn new(source_name: Arc<String>, lookup: Arc<Lookup>, location: Range<usize>) -> Self {
        Span {
            source_name,
            lookup,
            location,
        }
    }

    pub fn end(&self) -> usize {
        return self.location.end;
    }

    pub fn source_name(&self) -> Arc<String> {
        self.source_name.clone()
    }

    pub fn lookup(&self) -> Arc<Lookup> {
        self.lookup.clone()
    }

    pub fn to(&self, other: &Span) -> Span {
        debug_assert_eq!(self.source_name, other.source_name);
        debug_assert_eq!(self.lookup, other.lookup);

        Span {
            source_name: self.source_name.clone(),
            lookup: self.lookup.clone(),
            location: self.location.start.min(other.location.start)
                ..self.location.end.max(other.location.end),
        }
    }

    pub fn line_col(&self) -> (usize, usize) {
        self.lookup.line_col(self.location.start)
    }

    pub fn pointer(&self, arrow_color: Color) -> (String, usize) {
        let lines = self.lookup.lines(self.location.clone());
        let line_n = lines.start + 1;
        let col_n = self.lookup.col_from_line(lines.start, self.location.start) + 1;

        if lines.len() > 1 {
            todo!()
        } else {
            let line = self.lookup.line(lines.start).trim_end();
            let offset = (lines.start + 1).ilog10() as usize + 2;

            (format!(
                "\
                {arrow:>arr_space$} {name}:{line_n}:{col_n}\n\
                {cap:>width$}\n\
                {n} {line}\n\
                {cap:>width$} {pointer}\
                ",
                arrow = "-->".bright_blue().bold(),
                name = self.source_name,
                cap = "|".bright_blue().bold(),
                width = offset + 1,
                arr_space = offset + 2,
                n = format!("{line_n:<offset$}|").bright_blue().bold(),
                pointer = format!(
                    "{blank:>start$}{blank:^>length$}",
                    blank = "",
                    start = col_n - 1,
                    length = self.location.end - self.location.start,
                )
                .color(arrow_color),
            ), offset)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lookup {
    source: Arc<String>,
    heads: Box<[usize]>,
}

impl Lookup {
    pub fn new(source: Arc<String>) -> Self {
        // Tabs should be replaced with spaces in order to keep character spacing the same
        debug_assert!(!source.contains('\t'));

        let heads = std::iter::once(0)
            .chain(
                source
                    .char_indices()
                    .filter_map(|(i, c)| if c == '\n' { Some(i + 1) } else { None }),
            )
            .collect();

        Lookup { source, heads }
    }

    pub fn line_n(&self, index: usize) -> usize {
        match self.heads.binary_search(&index) {
            Ok(line) => line,
            Err(insert) => insert - 1,
        }
    }

    #[inline]
    pub fn line_col(&self, index: usize) -> (usize, usize) {
        let line = self.line_n(index);
        let col = self.col_from_line(line, index);

        (line, col)
    }

    #[inline]
    pub fn col_from_line(&self, line: usize, index: usize) -> usize {
        index - self.heads[line]
    }

    pub fn multiline(&self, range: Range<usize>) -> bool {
        let starting_line = self.line_n(range.start);
        let next_start = self.heads[starting_line + 1];

        range.end <= next_start
    }

    pub fn line(&self, index: usize) -> &str {
        let range = self.heads[index]..self.heads[index + 1];

        &self.source[range]
    }

    pub fn lines(&self, span: Range<usize>) -> Range<usize> {
        let start_line = self.line_n(span.start);
        let next_start = self.heads[start_line + 1];

        if span.end <= next_start {
            // Check if the span ends on the same line
            start_line..start_line + 1
        } else {
            // Otherwise perform a binary search through the rest of the lines.
            match self.heads[start_line + 1..].binary_search(&(span.end - 1)) {
                Ok(end_line) => start_line..end_line + 1,
                Err(insert) => start_line..insert,
            }
        }
    }
}
