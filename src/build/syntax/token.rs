use super::lex::{Delimeter, Keyword, Macro, Punctuation, Token};
use super::parse::{Cursor, Parsable};
use crate::build::ascii::AsciiStr;
use crate::build::symbol_table::SymbolRef;
use nurse::prelude::*;
use std::io::Stdout;

#[macro_export]
macro_rules! Token {
    [=] => {$crate::build::syntax::token::Eq};
    [==] => {$crate::build::syntax::token::EqEq};
    [!=] => {$crate::build::syntax::token::Ne};
    [<] => {$crate::build::syntax::token::Lt};
    [<=] => {$crate::build::syntax::token::Le};
    [>] => {$crate::build::syntax::token::Gt};
    [>=] => {$crate::build::syntax::token::Ge};
    [&] => {$crate::build::syntax::token::And};
    [&&] => {$crate::build::syntax::token::AndAnd};
    [|] => {$crate::build::syntax::token::Or};
    [||] => {$crate::build::syntax::token::OrOr};
    [^] => {$crate::build::syntax::token::Caret};
    [!] => {$crate::build::syntax::token::Not};
    [~] => {$crate::build::syntax::token::Not};
    [+] => {$crate::build::syntax::token::Plus};
    [-] => {$crate::build::syntax::token::Minus};
    [/] => {$crate::build::syntax::token::Slash};
    [*] => {$crate::build::syntax::token::Star};
    [<<] => {$crate::build::syntax::token::Shl};
    [>>] => {$crate::build::syntax::token::Shr};
    [,] => {$crate::build::syntax::token::Comma};
    [:] => {$crate::build::syntax::token::Colon};
    [;] => {$crate::build::syntax::token::Semicolon};
    [if] => {$crate::build::syntax::token::If};
    [else] => {$crate::build::syntax::token::Else};
    [fn] => {$crate::build::syntax::token::Fn};
    [return] => {$crate::build::syntax::token::Return};
    [pub] => {$crate::build::syntax::token::Pub};
    [const] => {$crate::build::syntax::token::Const};
    [static] => {$crate::build::syntax::token::Static};
    [progmem] => {$crate::build::syntax::token::Progmem};
    [mut] => {$crate::build::syntax::token::Mut};
    [let] => {$crate::build::syntax::token::Let};
    [for] => {$crate::build::syntax::token::For};
    [while] => {$crate::build::syntax::token::While};
    [break] => {$crate::build::syntax::token::Break};
    [continue] => {$crate::build::syntax::token::Continue};
    [subspace] => {$crate::build::syntax::token::Subspace};
    [as] => {$crate::build::syntax::token::As};
    ["("] => {$crate::build::syntax::token::OpenParen};
    [")"] => {$crate::build::syntax::token::CloseParen};
    ["["] => {$crate::build::syntax::token::OpenBracket};
    ["]"] => {$crate::build::syntax::token::CloseBracket};
    ["{"] => {$crate::build::syntax::token::OpenBrace};
    ["}"] => {$crate::build::syntax::token::CloseBrace};
}

macro_rules! parsable {
    ($($description:literal: $token:ident($inner:pat) => $name:ident),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name;

            impl Parsable for $name {
                async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
                    let next = cursor.next().map(Spanned::deconstruct);
                    match next {
                        Some((Token::$token($inner), _)) => Ok($name),
                        Some((tok, span)) => {
                            reporter.report(error!(span, "expected {}, found {}", $description, tok.description())).await;
                            Err(())
                        }
                        None => {
                            reporter.report(error!(cursor.eof(), "expected {}, found `EOF`", $description)).await;
                            Err(())
                        }
                    }
                }

                fn description(&self) -> &'static str {
                    $description
                }
            }

            impl Parsable for Spanned<$name> {
                async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
                    let next = cursor.next().map(Spanned::deconstruct);
                    match next {
                        Some((Token::$token($inner), span)) => Ok(Spanned::new($name, span)),
                        Some((tok, span)) => {
                            reporter.report(error!(span, "expected {}, found {}", $description, tok.description())).await;
                            Err(())
                        }
                        None => {
                            reporter.report(error!(cursor.eof(), "expected {}, found `EOF`", $description)).await;
                            Err(())
                        }
                    }
                }

                fn description(&self) -> &'static str {
                    $description
                }
            }

            impl Parsable for Option<$name> {
                async fn parse(cursor: &mut Cursor<'_>, _: &TerminalReporter<Stdout>) -> Result<Self, ()> {
                    let next = cursor.next().map(Spanned::into_inner);
                    match next {
                        Some(Token::$token($inner)) => Ok(Some($name)),
                        _ => Ok(None),
                    }
                }

                fn description(&self) -> &'static str {
                    $description
                }
            }
        )*
    };
    ($($description:literal: $token:ident($inner:pat) => $name:ident{$($v:vis $field:ident: $ty:ty)*}),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name {
                $(
                    $v $field: $ty,
                )*
            }

            impl Parsable for Spanned<$name> {
                async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
                    let next = cursor.next().map(Spanned::deconstruct);
                    match next {
                        Some((Token::$token($inner), span)) => Ok(Spanned::new($name { $($field)* }, span)),
                        Some((tok, span)) => {
                            reporter.report(error!(span, "expected {}, found {}", $description, tok.description())).await;
                            Err(())
                        }
                        None => {
                            reporter.report(error!(cursor.eof(), "expected {}, found `EOF`", $description)).await;
                            Err(())
                        }
                    }
                }

                fn description(&self) -> &'static str {
                    $description
                }
            }
        )*
    };
}

//------ Delimeters ------//

parsable! {
    "`(`" : Delimeter(Delimeter::OpenParen) => OpenParen,
    "`)`"  : Delimeter(Delimeter::CloseParen) => CloseParen,
    "`[`"  : Delimeter(Delimeter::OpenBracket) => OpenBracket,
    "`]`"  : Delimeter(Delimeter::CloseBracket) => CloseBracket,
    "`{{`" : Delimeter(Delimeter::OpenBrace) => OpenBrace,
    "`}}`" : Delimeter(Delimeter::CloseBrace) => CloseBrace,
}

//------ Punctuation ------//

parsable! {
    "`=`"  : Punctuation(Punctuation::Eq) => Eq,
    "`==`" : Punctuation(Punctuation::EqEq) => EqEq,
    "`!=`" : Punctuation(Punctuation::NotEqual) => Ne,
    "`<`"  : Punctuation(Punctuation::Lt) => Lt,
    "`<=`" : Punctuation(Punctuation::Le) => Le,
    "`>`"  : Punctuation(Punctuation::Gt) => Gt,
    "`>=`" : Punctuation(Punctuation::Ge) => Ge,
    "`&`"  : Punctuation(Punctuation::And) => And,
    "`&&`" : Punctuation(Punctuation::AndAnd) => AndAnd,
    "`|`"  : Punctuation(Punctuation::Pipe) => Pipe,
    "`||`" : Punctuation(Punctuation::PipePipe) => PipePipe,
    "`^`"  : Punctuation(Punctuation::Caret) => Caret,
    "`!`"  : Punctuation(Punctuation::Not) => Not,
    "`+`"  : Punctuation(Punctuation::Plus) => Plus,
    "`-`"  : Punctuation(Punctuation::Minus) => Minus,
    "`/`"  : Punctuation(Punctuation::Slash) => Slash,
    "`*`"  : Punctuation(Punctuation::Star) => Star,
    "`%`"  : Punctuation(Punctuation::Percent) => Percent,
    "`+=`" : Punctuation(Punctuation::PlusEq) => PlusEq,
    "`-=`" : Punctuation(Punctuation::MinusEq) => MinusEq,
    "`*=`" : Punctuation(Punctuation::MulEq) => MulEq,
    "`/=`" : Punctuation(Punctuation::DivEq) => DivEq,
    "`%=`" : Punctuation(Punctuation::ModEq) => ModEq,
    "`<<`" : Punctuation(Punctuation::Shl) => Shl,
    "`>>`" : Punctuation(Punctuation::Shr) => Shr,
    "`,`"  : Punctuation(Punctuation::Comma) => Comma,
    "`;`"  : Punctuation(Punctuation::Semicolon) => Semicolon,
    "`:`"  : Punctuation(Punctuation::Colon) => Colon,
    "`::`" : Punctuation(Punctuation::DoubleColon) => DoubleColon,
}

//------ Keywords ------//

parsable! {
    "keyword `import`" : Keyword(Keyword::Import) => Import,
    "keyword `fn`" : Keyword(Keyword::Fn) => Fn,
    "keyword `return`" : Keyword(Keyword::Return) => Return,
    "keyword `pub`" : Keyword(Keyword::Pub) => Pub,
    "keyword `const`" : Keyword(Keyword::Const) => Const,
    "keyword `static`" : Keyword(Keyword::Static) => Static,
    "keyword `progmem`" : Keyword(Keyword::Progmem) => Progmem,
    "keyword `mut`" : Keyword(Keyword::Mut) => Mut,
    "keyword `let`" : Keyword(Keyword::Let) => Let,
    "keyword `if`" : Keyword(Keyword::If) => If,
    "keyword `else`" : Keyword(Keyword::Else) => Else,
    "keyword `for`" : Keyword(Keyword::For) => For,
    "keyword `while`" : Keyword(Keyword::While) => While,
    "keyword `break`" : Keyword(Keyword::Break) => Break,
    "keyword `continue`" : Keyword(Keyword::Continue) => Continue,
    "keyword `subspace`" : Keyword(Keyword::Subspace) => Subspace,
    "keyword `as`" : Keyword(Keyword::As) => As,
}

//-------- Macros --------//

parsable! {
    "macro `sizeof`" : Macro(Macro::Sizeof) => Sizeof,
    "macro `asm`" : Macro(Macro::Asm) => Asm,
}

//------ Containers ------//

parsable! {
    "integer" : Immediate(value) => Immediate { pub value: i128 },
    "string" : String(value) => LitString { pub value: AsciiStr },
}

// Declare this seperately in order to implement `Eq`, `Copy`, and `Hash` for `Ident`
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub symbol: SymbolRef,
}

impl Parsable for Spanned<Ident> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let next = cursor.next().map(Spanned::deconstruct);
        match next {
            Some((Token::Ident(inner), span)) => Ok(Spanned::new(Ident { symbol: inner }, span)),
            Some((tok, span)) => {
                reporter.report(error!(
                    span,
                    "expected identifier, found {}",
                    tok.description()
                )).await;
                Err(())
            },
            None => {
                reporter.report(error!(
                    cursor.eof(),
                    "expected identifier, found `EOF`",
                )).await;
                Err(())
            }
        }
    }

    fn description(&self) -> &'static str {
        "identifier"
    }
}
