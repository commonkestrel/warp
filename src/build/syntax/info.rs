use logos::Logos;

use crate::{debug, diagnostic::{Diagnostic, Reporter}, span::{Span, Spanned}, spanned_error};

use super::token::LitString;

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
    Simple(String),
    Git {
        url: String, 
        commit: Option<String>,
        branch: Option<String>,
    },
    Path(String),
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
    #[regex(r"[\x21-\x27\x2A-\x3C\x3E-\x7F]+", |lex| lex.slice().to_owned())]
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
            InfoToken::Text(_) => "text",
        }
    }
}

impl CompInfo {
    pub fn parse(source: Spanned<String>, reporter: &Reporter) -> Spanned<CompInfo> {
        let mut tokens = Vec::new();

        let src = source.inner().to_string();
        let mut lex = InfoToken::lexer(&src);
        let global_span = source.span();

        while let Some(tok) = lex.next() {
            let location = (global_span.start() + lex.span().start)..(global_span.start() + lex.span().end);
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
            Some(Spanned {inner: InfoToken::Lib, ..}) => {
                let eol_location = (global_span.end()-1)..global_span.end();
                let eol_span = global_span.clone().with_location(eol_location);

                match CompInfo::parse_lib(&tokens[1..], eol_span) {
                    Ok(lib) => {
                        let (library, span) = lib.deconstruct();
                        Spanned::new(CompInfo::Lib(library), span)
                    }
                    Err(err) => {
                        reporter.report_sync(err);
                        Spanned::new(CompInfo::Err, source.into_span())
                    }
                }
            }
            Some(tok) => {
                reporter.report_sync(spanned_error!(global_span.clone(), "expected compiler info indicator, found `{}`", tok.description()).with_note("consider starting compiler info with one of the following: `lib`"));
                return Spanned::new(CompInfo::Err, tok.span().clone());
            }
            None => {
                reporter.report_sync(spanned_error!(global_span.clone(), "expected compiler info, found `EOL`"));
                return Spanned::new(CompInfo::Err, source.into_span());
            }
        }
    }

    fn parse_lib(tokens: &[Spanned<InfoToken>], eol_span: Span) -> Result<Spanned<Lib>, Diagnostic> {
        let ident = expect_text(&eol_span, tokens.get(0))?;
        expect_token(&InfoToken::Equal, &eol_span, tokens.get(1))?;

        match tokens.get(2) {
            Some(Spanned {inner: InfoToken::Git, span}) => {
                expect_token(&InfoToken::OpenParen, &eol_span, tokens.get(3))?;
                let mut url: Option<String> = None;
                let mut commit: Option<String> = None;
                let mut branch: Option<String> = None;

                let mut i = 3;
                while let Some(tok) = tokens.get(i) {
                    if tok.inner() == &InfoToken::CloseParen {
                        break;
                    }

                    let field = expect_text(&eol_span, Some(tok))?;
                    expect_token(&InfoToken::Equal, &eol_span, tokens.get(i+1))?;
                }

                let url = match url {
                    Some(u) => u,
                    None => return Err(spanned_error!(span.clone(), "git sources require a `url` parameter")),
                };

                todo!()
            }
            Some(Spanned {inner: InfoToken::Path, span}) => {
                expect_token(&InfoToken::OpenParen, &eol_span, tokens.get(3))?;
                let (path, span) = expect_text(&eol_span, tokens.get(4))?.deconstruct();
                expect_token(&InfoToken::CloseParen, &eol_span, tokens.get(5))?;

                let info_span = ident.span().to(&span);
                Ok(Spanned::new(Lib {
                    ident, src: Spanned::new(LibSrc::Path(path), span),
                }, info_span))
            }
            Some(Spanned {inner: InfoToken::Text(url), span}) => {
                let info_span = ident.span().to(span);

                Ok(Spanned::new(Lib {
                    ident,
                    src: Spanned::new(LibSrc::Simple(url.clone()), span.clone())
                }, info_span))
            }
            Some(tok) => return Err(spanned_error!(tok.span().clone(), "expected library source, found {}", tok.description())),
            None => return Err(spanned_error!(eol_span, "expected library source, found `EOL`")),
        }
    }
}

fn expect_token(target: &InfoToken, eol_span: &Span, tok: Option<&Spanned<InfoToken>>) -> Result<Span, Diagnostic> {
    match tok {
        Some(Spanned {inner: tok, span}) => {
            if tok == target {
                Ok(span.clone())
            } else {
                Err(spanned_error!(span.clone(), "expected library identifier, found {}", tok.description()))
            }
        },
        None => Err(spanned_error!(eol_span.clone(), "expected library identifier, found `EOL`")),
    }
}

fn expect_text(eol_span: &Span, tok: Option<&Spanned<InfoToken>>) -> Result<Spanned<String>, Diagnostic> {
    match tok {
        Some(Spanned {inner: InfoToken::Text(txt), span}) => Ok(Spanned::new(txt.clone(), span.clone())),
        Some(Spanned {inner: tok, span}) => return Err(spanned_error!(span.clone(), "expected library identifier, found {}", tok.description())),
        None => return Err(spanned_error!(eol_span.clone(), "expected library identifier, found `EOL`")),
    }
}
