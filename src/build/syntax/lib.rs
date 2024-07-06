use logos::Logos;

use crate::{diagnostic::{Diagnostic, Reporter}, span::Spanned, spanned_error};

use super::token::LitString;

#[derive(Debug, Clone)]
pub enum LibSrc {
    Simple(String),
    Git {
        url: String, 
        commit: Option<String>,
        branch: Option<String>,
    },
    Path(String),
    Err,
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

impl LibSrc {
    pub fn parse(source: Spanned<LitString>, reporter: &Reporter) -> Spanned<LibSrc> {
        let mut tokens = Vec::new();

        let src = source.inner().value.to_string();
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
            Some(tok) => {
                reporter.report_sync(spanned_error!(global_span.clone(), "expected compiler info indicator, found `{}`", tok.description()).with_note("consider starting compiler info with one of the following: `lib`"));
                Spanned::new(LibSrc::Err, source.into_span())
            }
            None => {
                reporter.report_sync(spanned_error!(global_span.clone(), "expected compiler info, found `EOL`"));
                Spanned::new(LibSrc::Err, source.into_span())
            }
        }
    }
}
