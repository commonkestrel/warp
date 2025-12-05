use std::{io::Stdout, path::PathBuf};
use logos::Logos;
use url::Url;
use nurse::prelude::*;

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
#[logos(error = String)]
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
    pub async fn parse(source: Spanned<String>, reporter: &TerminalReporter<Stdout>) -> CompInfo {
        let mut tokens = Vec::new();

        let src = source.inner().to_string();
        let mut lex = InfoToken::lexer(&src);
        let global_span = source.span();

        while let Some(tok) = lex.next() {
            let location = (global_span.start() + lex.span().start + 3)
                ..(global_span.start() + lex.span().end + 3);
            let s = Span::new(global_span.lookup(), location);

            match tok {
                Ok(tok) => {
                    let token = Spanned::new(tok, s);
                    tokens.push(token);
                }
                Err(mut err) => reporter.report(Diagnostic::spanned_error(s, err)).await,
            }
        }

        match tokens.get(0).map(Spanned::inner) {
            Some(InfoToken::Lib) => {
                let eol_location = (global_span.end() - 1)..global_span.end();
                let eol_span = Span::new(global_span.lookup(), eol_location);

                match CompInfo::parse_lib(&tokens[1..], eol_span, reporter).await {
                    Ok(lib) => {
                        let library = lib.into_inner();
                        CompInfo::Lib(library)
                    }
                    Err(_) => CompInfo::Err
                }
            }
            Some(tok) => {
                reporter.report(
                    error!(
                        global_span,
                        "expected compiler info indicator, found `{}`",
                        tok.description()
                    )
                    .with_note("consider starting compiler info with one of the following: `lib`"),
                ).await;
                return CompInfo::Err;
            }
            None => {
                reporter.report(error!(
                    global_span,
                    "expected compiler info, found `EOL`"
                )).await;
                return CompInfo::Err;
            }
        }
    }

    async fn parse_lib(
        tokens: &[Spanned<InfoToken>],
        eol_span: Span,
        reporter: &TerminalReporter<Stdout>
    ) -> Result<Spanned<Lib>, ()> {
        let ident = expect_text(&eol_span, tokens.get(0), reporter).await?;
        expect_token(&InfoToken::Equal, &eol_span, tokens.get(1), reporter).await?;

        match tokens.get(2).map(Spanned::inner) {
            Some(InfoToken::Git) => {
                expect_token(&InfoToken::OpenParen, &eol_span, tokens.get(3), reporter).await?;
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
                        reporter.report(error!(tokens[2].span(), "missing seperator")
                            .with_note("consider adding `,` here")).await;
                        return Err(());
                    }

                    let field = expect_text(&eol_span, Some(tok), reporter).await?;
                    expect_token(&InfoToken::Equal, &eol_span, tokens.get(i + 1), reporter).await?;
                    let value = expect_text(&eol_span, tokens.get(i + 2), reporter).await?;
                    if let Some(InfoToken::Comma) = tokens.get(i + 3).map(Spanned::inner) {
                        comma = true;
                        i += 4;
                    } else {
                        comma = false;
                        i += 3;
                    }

                    match field.inner().as_str() {
                        "url" => match Url::parse(&value) {
                            Ok(val) => url = Some(Spanned::new(val, value.span())),
                            Err(err) => {
                                reporter.report(error!(value.span(), "invalid URL: {err}")).await;
                                return Err(());
                            }
                        },
                        "commit" => commit = Some(value),
                        "branch" => branch = Some(value),
                        _ => {
                            let (description, span) = field.deconstruct();
                            reporter.report(error!(span, "unknown field {description}")).await;
                            return Err(())
                        }
                    }
                }

                let close = expect_token(&InfoToken::CloseParen, &eol_span, tokens.get(i), reporter).await?;
                let span = tokens[2].span();

                let url = match url {
                    Some(u) => u,
                    None => {
                        reporter.report(error!(
                            span,
                            "git sources require a `url` parameter"
                        )).await;
                        return Err(())
                    }
                };

                let info_span = ident.span().to(close);
                let src_span = span.to(close);

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
            Some(InfoToken::Path) => {
                expect_token(&InfoToken::OpenParen, &eol_span, tokens.get(3), reporter).await?;
                let (path, span) = expect_text(&eol_span, tokens.get(4), reporter).await?.deconstruct();
                expect_token(&InfoToken::CloseParen, &eol_span, tokens.get(5), reporter).await?;

                let info_span = ident.span().to(span);
                Ok(Spanned::new(
                    Lib {
                        ident,
                        src: Spanned::new(LibSrc::Path(PathBuf::from(path)), span),
                    },
                    info_span,
                ))
            }
            Some(InfoToken::Text(value)) => {
                let span = tokens[2].span();
                let info_span = ident.span().to(span);
                let url = match Url::parse(&value) {
                    Ok(url) => url,
                    Err(err) => {
                        reporter.report(error!(span.clone(), "invalid URL: {err}")).await;
                        return Err(());
                    }
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
                reporter.report(error!(
                    tokens[2].span(),
                    "expected library source, found {}",
                    tok.description()
                )).await;
                return Err(());
            }
            None => {
                reporter.report(error!(
                    eol_span,
                    "expected library source, found `EOL`"
                )).await;
                return Err(());
            }
        }
    }
}

async fn expect_token(
    target: &InfoToken,
    eol_span: &Span,
    tok: Option<&Spanned<InfoToken>>,
    reporter: &TerminalReporter<Stdout>,
) -> Result<Span, ()> {
    match tok {
        Some(tok) => {
            if tok.inner() == target {
                Ok(tok.span())
            } else {
                reporter.report(error!(
                    tok.span(),
                    "expected {}, found {}",
                    target.description(),
                    tok.inner().description()
                )).await;
                Err(())
            }
        }
        None => {
            reporter.report(error!(
                eol_span.clone(),
                "expected {}, found `EOL`",
                target.description()
            )).await;
            Err(())
        },
    }
}

async fn expect_text(
    eol_span: &Span,
    tok: Option<&Spanned<InfoToken>>,
    reporter: &TerminalReporter<Stdout>
) -> Result<Spanned<String>, ()> {
    match tok.map(Spanned::inner) {
        Some(InfoToken::Text(txt)) => Ok(Spanned::new(txt.clone(), tok.unwrap().span())),
        Some(inner) => {
            reporter.report(error!(
                tok.unwrap().span(),
                "expected text, found {}",
                inner.description()
            )).await;
            return Err(())
        }
        None => {
            reporter.report(error!(
                eol_span,
                "expected text, found `EOL`"
            )).await;
            return Err(())
        }
    }
}
