use async_std::path::PathBuf;
use logos::Logos;
use url::Url;

use crate::{
    diagnostic::{Diagnostic, Reporter},
    span::{Span, Spanned},
    spanned_error,
};

#[derive(Debug, Clone)]
pub enum CompInfo {
    Lib(Lib),
    Err,
}

#[derive(Debug, Clone)]
pub struct Lib {
    pub ident: Spanned<String>,
    pub src: Spanned<LibSrc>,
}

#[derive(Debug, Clone)]
pub enum LibSrc {
    Simple(Url),
    Git {
        url: Spanned<Url>,
        commit: Option<Spanned<String>>,
        branch: Option<Spanned<String>>,
    },
    Path(PathBuf),
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = Diagnostic)]
#[logos(skip r"[ \t]")]
enum InfoToken {
    #[token("lib")]
    Lib,
    #[token("git")]
    Git,
    #[token("path")]
    Path,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("=")]
    Equal,
    #[token(",")]
    Comma,
    #[regex(r"[\x21-\x27\x2A-\x2B\x2D-\x3C\x3E-\x7F]+", |lex| lex.slice().to_owned())]
    Text(String),
}

impl InfoToken {
    fn description(&self) -> &'static str {
        match self {
            InfoToken::Lib => "`lib`",
            InfoToken::Git => "`git`",
            InfoToken::Path => "`path`",
            InfoToken::OpenParen => "`(`",
            InfoToken::CloseParen => "`)`",
            InfoToken::Equal => "`=`",
            InfoToken::Comma => "`,`",
            InfoToken::Text(_) => "text",
        }
    }
}

impl CompInfo {
    pub fn parse(source: Spanned<String>, reporter: &Reporter) -> CompInfo {
        let mut tokens = Vec::new();

        let src = source.inner().to_string();
        let mut lex = InfoToken::lexer(&src);
        let global_span = source.span();

        while let Some(tok) = lex.next() {
            let location = (global_span.start() + lex.span().start + 3)
                ..(global_span.start() + lex.span().end + 3);
            let s = global_span.clone().with_location(location);

            match tok {
                Ok(tok) => {
                    let token = Spanned::new(tok, s);
                    tokens.push(token);
                }
                Err(mut err) => {
                    err.set_span(Some(s));
                    reporter.report_sync(err);
                }
            }
        }

        match tokens.get(0) {
            Some(Spanned {
                inner: InfoToken::Lib,
                ..
            }) => {
                let eol_location = (global_span.end() - 1)..global_span.end();
                let eol_span = global_span.clone().with_location(eol_location);

                match CompInfo::parse_lib(&tokens[1..], eol_span) {
                    Ok(lib) => {
                        let (library, span) = lib.deconstruct();
                        CompInfo::Lib(library)
                    }
                    Err(err) => {
                        reporter.report_sync(err);
                        CompInfo::Err
                    }
                }
            }
            Some(tok) => {
                reporter.report_sync(
                    spanned_error!(
                        global_span.clone(),
                        "expected compiler info indicator, found `{}`",
                        tok.description()
                    )
                    .with_note("consider starting compiler info with one of the following: `lib`"),
                );
                return CompInfo::Err;
            }
            None => {
                reporter.report_sync(spanned_error!(
                    global_span.clone(),
                    "expected compiler info, found `EOL`"
                ));
                return CompInfo::Err;
            }
        }
    }

    fn parse_lib(
        tokens: &[Spanned<InfoToken>],
        eol_span: Span,
    ) -> Result<Spanned<Lib>, Diagnostic> {
        let ident = expect_text(&eol_span, tokens.get(0))?;
        expect_token(&InfoToken::Equal, &eol_span, tokens.get(1))?;

        match tokens.get(2) {
            Some(Spanned {
                inner: InfoToken::Git,
                span,
            }) => {
                expect_token(&InfoToken::OpenParen, &eol_span, tokens.get(3))?;
                let mut url: Option<Spanned<Url>> = None;
                let mut commit: Option<Spanned<String>> = None;
                let mut branch: Option<Spanned<String>> = None;

                let mut i = 4;
                let mut comma = true;
                while let Some(tok) = tokens.get(i) {
                    if tok.inner() == &InfoToken::CloseParen {
                        break;
                    }

                    if !comma {
                        return Err(spanned_error!(tok.span().clone(), "missing seperator")
                            .with_note("consider adding `,` here"));
                    }

                    let field = expect_text(&eol_span, Some(tok))?;
                    expect_token(&InfoToken::Equal, &eol_span, tokens.get(i + 1))?;
                    let value = expect_text(&eol_span, tokens.get(i + 2))?;
                    if let Some(InfoToken::Comma) = tokens.get(i + 3).map(Spanned::inner) {
                        comma = true;
                        i += 4;
                    } else {
                        comma = false;
                        i += 3;
                    }

                    match field.inner().as_str() {
                        "url" => match Url::parse(&value) {
                            Ok(val) => url = Some(Spanned::new(val, value.into_span())),
                            Err(err) => {
                                return Err(spanned_error!(value.into_span(), "invalid URL: {err}"))
                            }
                        },
                        "commit" => commit = Some(value),
                        "branch" => branch = Some(value),
                        _ => {
                            let (description, span) = field.deconstruct();
                            return Err(spanned_error!(span, "unknown field {description}"));
                        }
                    }
                }

                let close = expect_token(&InfoToken::CloseParen, &eol_span, tokens.get(i))?;

                let url = match url {
                    Some(u) => u,
                    None => {
                        return Err(spanned_error!(
                            span.clone(),
                            "git sources require a `url` parameter"
                        ))
                    }
                };

                let info_span = ident.span().to(&close);
                let src_span = span.to(&close);

                Ok(Spanned::new(
                    Lib {
                        ident,
                        src: Spanned::new(
                            LibSrc::Git {
                                url,
                                commit,
                                branch,
                            },
                            src_span,
                        ),
                    },
                    info_span,
                ))
            }
            Some(Spanned {
                inner: InfoToken::Path,
                span,
            }) => {
                expect_token(&InfoToken::OpenParen, &eol_span, tokens.get(3))?;
                let (path, span) = expect_text(&eol_span, tokens.get(4))?.deconstruct();
                expect_token(&InfoToken::CloseParen, &eol_span, tokens.get(5))?;

                let info_span = ident.span().to(&span);
                Ok(Spanned::new(
                    Lib {
                        ident,
                        src: Spanned::new(LibSrc::Path(PathBuf::from(path)), span),
                    },
                    info_span,
                ))
            }
            Some(Spanned {
                inner: InfoToken::Text(value),
                span,
            }) => {
                let info_span = ident.span().to(span);
                let url = match Url::parse(&value) {
                    Ok(url) => url,
                    Err(err) => return Err(spanned_error!(span.clone(), "invalid URL: {err}")),
                };

                Ok(Spanned::new(
                    Lib {
                        ident,
                        src: Spanned::new(LibSrc::Simple(url), span.clone()),
                    },
                    info_span,
                ))
            }
            Some(tok) => {
                return Err(spanned_error!(
                    tok.span().clone(),
                    "expected library source, found {}",
                    tok.description()
                ))
            }
            None => {
                return Err(spanned_error!(
                    eol_span,
                    "expected library source, found `EOL`"
                ))
            }
        }
    }
}

fn expect_token(
    target: &InfoToken,
    eol_span: &Span,
    tok: Option<&Spanned<InfoToken>>,
) -> Result<Span, Diagnostic> {
    match tok {
        Some(Spanned { inner: tok, span }) => {
            if tok == target {
                Ok(span.clone())
            } else {
                Err(spanned_error!(
                    span.clone(),
                    "expected {}, found {}",
                    target.description(),
                    tok.description()
                ))
            }
        }
        None => Err(spanned_error!(
            eol_span.clone(),
            "expected {}, found `EOL`",
            target.description()
        )),
    }
}

fn expect_text(
    eol_span: &Span,
    tok: Option<&Spanned<InfoToken>>,
) -> Result<Spanned<String>, Diagnostic> {
    match tok {
        Some(Spanned {
            inner: InfoToken::Text(txt),
            span,
        }) => Ok(Spanned::new(txt.clone(), span.clone())),
        Some(Spanned { inner: tok, span }) => {
            return Err(spanned_error!(
                span.clone(),
                "expected text, found {}",
                tok.description()
            ))
        }
        None => {
            return Err(spanned_error!(
                eol_span.clone(),
                "expected text, found `EOL`"
            ))
        }
    }
}
