use std::io::Stdout;

use crate::{
    build::ascii::AsciiStr,
    seek, Token,
};

use nurse::prelude::*;

use super::{
    lex::{Delimeter, Keyword, Macro, Primitive, Punctuation, Token},
    parse::{Cursor, Parenthesized, Parsable, Punctuated},
    token::{Ident, LitString},
};

#[derive(Debug, Clone)]
pub struct Const {
    pub ident: Spanned<Ident>,
    pub value: Spanned<Expr>,
}

impl Parsable for Spanned<Const> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let keyword: Spanned<Token![const]> = cursor.parse(reporter).await?;
        let ident: Spanned<Ident> = cursor.parse(reporter).await?;
        let _: Token![=] = cursor.parse(reporter).await?;
        let value = Expr::parse_assignment(cursor, reporter).await;

        let const_span = keyword.span().to(value.span());
        Ok(Spanned::new(Const { ident, value }, const_span))
    }

    fn description(&self) -> &'static str {
        "constant"
    }
}

#[derive(Debug, Clone)]
pub struct Static {
    pub ident: Spanned<Ident>,
    pub ty: Spanned<Type>,
    pub value: Spanned<Expr>,
}

impl Parsable for Spanned<Static> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let keyword: Spanned<Token![static]> = cursor.parse(reporter).await?;
        let ident: Spanned<Ident> = cursor.parse(reporter).await?;

        let _: Token![:] = cursor.parse(reporter).await?;
        let ty = Type::parse(cursor, reporter).await;

        let _: Token![=] = cursor.parse(reporter).await?;
        let value = Expr::parse_assignment(cursor, reporter).await;

        let static_span = keyword.span().to(value.span());
        Ok(Spanned::new(Static { ident, ty, value }, static_span))
    }

    fn description(&self) -> &'static str {
        "static variable"
    }
}

#[derive(Debug, Clone)]
pub struct Progmem {
    pub ident: Spanned<Ident>,
    pub ty: Spanned<Type>,
    pub value: Spanned<Expr>,
}

impl Parsable for Spanned<Progmem> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let keyword: Spanned<Token![progmem]> = cursor.parse(reporter).await?;
        let ident: Spanned<Ident> = cursor.parse(reporter).await?;

        let _: Token![:] = cursor.parse(reporter).await?;
        let ty = Type::parse(cursor, reporter).await;

        let _: Token![=] = cursor.parse(reporter).await?;
        let value = Expr::parse_assignment(cursor, reporter).await;

        let static_span = keyword.span().to(value.span());
        Ok(Spanned::new(Progmem { ident, ty, value }, static_span))
    }

    fn description(&self) -> &'static str {
        "program memory variable"
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub ident: Spanned<Ident>,
    pub parameters: Punctuated<Spanned<Parameter>, Token![,]>,
    pub return_type: Spanned<Type>,
    pub body: Spanned<Statement>,
}

impl Parsable for Spanned<Function> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let keyword: Spanned<Token![fn]> = cursor.parse(reporter).await?;
        let ident = cursor.parse(reporter).await?;
        let _: Token!["("] = cursor.parse(reporter).await?;

        let mut params_inner = Vec::new();
        let mut last_param = None;

        while let Some(tok) = cursor.peek() {
            match tok.inner() {
                Token::Delimeter(Delimeter::CloseParen) => break,
                Token::Punctuation(Punctuation::Comma) => match last_param.take() {
                    Some(param) => {
                        cursor.step();
                        params_inner.push((param, Token![,]))
                    }
                    None => {
                        reporter.report(error!(
                            tok.span(),
                            "unexpected duplicate seperator"
                        )).await;
                        cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                        break;
                    }
                },
                _ => {
                    let param = match cursor.parse(reporter).await {
                        Ok(param) => param,
                        Err(err) => {
                            cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                            break;
                        }
                    };

                    last_param = Some(param);
                }
            }
        }

        let parameters = Punctuated::new(params_inner, last_param);

        let close_paren: Spanned<Token![")"]> = cursor.parse(reporter).await?;

        let return_type = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();
            Type::parse(cursor, reporter).await
        } else {
            reporter.report(error!(
                close_paren.span(),
                "missing return type"
            )).await;
            Spanned::new(Type::Err, close_paren.span())
        };

        let body = Statement::parse(cursor, reporter).await;

        let fn_span = keyword.span().to(body.span());
        Ok(Spanned::new(
            Function {
                ident,
                parameters,
                return_type,
                body,
            },
            fn_span,
        ))
    }

    fn description(&self) -> &'static str {
        "function definition"
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub mutability: Mutability,
    pub ident: Spanned<Ident>,
    pub ty: Spanned<Type>,
}

impl Parsable for Spanned<Parameter> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let mutability = if cursor.check(&Token::Keyword(Keyword::Mut)) {
            cursor.step();
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };

        let ident: Spanned<Ident> = cursor.parse(reporter).await?;
        let _: Token![:] = cursor.parse(reporter).await?;
        let ty = Type::parse(cursor, reporter).await;

        let param_span = ident.span().to(ty.span());
        Ok(Spanned::new(
            Parameter {
                mutability,
                ident,
                ty,
            },
            param_span,
        ))
    }

    fn description(&self) -> &'static str {
        "function parameter"
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    Block(Vec<Spanned<Statement>>),
    If {
        condition: Parenthesized<Spanned<Expr>>,
        content: Box<Spanned<Statement>>,
        else_block: Option<Box<Spanned<Statement>>>,
    },
    For(Box<ForLoop>),
    While(Box<WhileLoop>),
    Break,
    Continue,
    Return(Option<Spanned<Expr>>),
    Asm(Parenthesized<Spanned<LitString>>),
    Var {
        mutability: Mutability,
        ident: Spanned<Ident>,
        ty: Option<Spanned<Type>>,
        assignment: Spanned<Expr>,
    },
    Err,
}

macro_rules! inline_unwrap {
    ($enum:ident, $cursor:expr, Result = $expr:expr) => {
        match $expr {
            Ok(ok) => ok,
            Err(_) => return Spanned::new($enum::Err, $cursor.eof()),
        }
    };
    ($enum:ident, $cursor:expr, Option = $expr:expr) => {
        match $expr {
            Some(some) => some,
            None => return Spanned::new($enum::Err, $cursor.eof()),
        }
    };
}

impl Statement {
    fn requires_semicolon(&self) -> bool {
        match self {
            Statement::Expr(_)
            | Statement::Return(_)
            | Statement::Break
            | Statement::Continue
            | Statement::Asm(_)
            | Statement::Var { .. } => true,
            _ => false,
        }
    }

    pub async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Statement> {
        let (tok, span) = inline_unwrap!(
            Statement,
            cursor,
            Option = cursor.next()
        )
        .deconstruct();

        match tok {
            Token::Keyword(Keyword::If) => {
                let condition = inline_unwrap!(Statement, cursor, Result = cursor.parse(reporter).await);

                let content: Spanned<Statement> =
                    inline_unwrap!(Statement, cursor, Result = Box::pin(cursor.parse(reporter)).await);

                let (else_clause, if_span) = if cursor.check(&Token::Keyword(Keyword::Else)) {
                    let else_clause: Spanned<Statement> =
                        inline_unwrap!(Statement, cursor, Result = Box::pin(cursor.parse(reporter)).await);
                    let if_span = span.to(else_clause.span());

                    (Some(Box::new(else_clause)), if_span)
                } else {
                    (None, span.to(content.span()))
                };

                Spanned::new(
                    Statement::If {
                        condition,
                        content: Box::new(content),
                        else_block: else_clause,
                    },
                    if_span,
                )
            }
            Token::Keyword(Keyword::For) => {
                let header = inline_unwrap!(Statement, cursor, Result = Box::pin(cursor.parse(reporter)).await);
                let content = Box::pin(Statement::parse(cursor, reporter)).await;

                let for_span = span.to(content.span());
                let for_loop = ForLoop { header, content };

                Spanned::new(Statement::For(Box::new(for_loop)), for_span)
            }
            Token::Keyword(Keyword::While) => {
                let check = inline_unwrap!(Statement, cursor, Result = Box::pin(cursor.parse(reporter)).await);
                let contents = Box::pin(Statement::parse(cursor, reporter)).await;

                let while_span = span.to(contents.span());
                let while_loop = WhileLoop { check, contents };

                Spanned::new(Statement::While(Box::new(while_loop)), while_span)
            }
            Token::Keyword(Keyword::Break) => Spanned::new(Statement::Break, span),
            Token::Keyword(Keyword::Continue) => Spanned::new(Statement::Continue, span),
            Token::Keyword(Keyword::Return) => {
                if cursor.check(&Token::Punctuation(Punctuation::Semicolon)) {
                    return Spanned::new(Statement::Return(None), span);
                }
                let value = Expr::parse_assignment(cursor, reporter).await;

                let return_span = span.to(value.span());
                Spanned::new(Statement::Return(Some(value)), return_span)
            }
            Token::Delimeter(Delimeter::OpenBrace) => {
                let mut statements = Vec::new();

                while let Some(tok) = cursor.peek() {
                    match tok.inner() {
                        Token::Punctuation(Punctuation::Semicolon) => cursor.step(),
                        Token::Delimeter(Delimeter::CloseBrace) => break,
                        _ => {
                            let statement = Box::pin(Statement::parse(cursor, reporter)).await;
                            if statement.requires_semicolon() {
                                cursor.expect_semicolon(reporter).await;
                            }

                            statements.push(statement);
                        }
                    }
                }

                let close: Spanned<Token!["}"]> = match cursor.parse(reporter).await {
                    Ok(close) => close,
                    Err(_) => {
                        cursor.step_back();

                        reporter
                            .report(error!(span, "unmatched opening brace")).await;
                        return Spanned::new(Statement::Err, span);
                    }
                };

                let block_span = span.to(close.span());
                Spanned::new(Statement::Block(statements), block_span)
            }
            Token::Macro(Macro::Asm) => {
                let string: Parenthesized<Spanned<LitString>> =
                    inline_unwrap!(Statement, cursor, Result = cursor.parse(reporter).await);
                let asm_span = span.to(string.span());

                Spanned::new(Statement::Asm(string), asm_span)
            }
            Token::Keyword(Keyword::Let) => {
                Statement::variable(cursor, span, Mutability::Immutable, reporter).await
            }
            Token::Keyword(Keyword::Mut) => Statement::variable(cursor, span, Mutability::Mutable, reporter).await,
            _ => {
                cursor.step_back();

                let (expr, span) = Expr::parse_assignment(cursor, reporter).await.deconstruct();
                Spanned::new(Statement::Expr(expr), span)
            }
        }
    }

    async fn variable(
        cursor: &mut Cursor<'_>,
        keyword_span: Span,
        mutability: Mutability,
        reporter: &TerminalReporter<Stdout>
    ) -> Spanned<Statement> {
        let ident = inline_unwrap!(Statement, cursor, Result = cursor.parse(reporter).await);

        let ty = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();
            Some(Type::parse(cursor, reporter).await)
        } else {
            None
        };

        let _: Token![=] = inline_unwrap!(Statement, cursor, Result = cursor.parse(reporter).await);
        let assignment = Expr::parse_assignment(cursor, reporter).await;

        let var_span = keyword_span.to(assignment.span());
        Spanned::new(
            Statement::Var {
                mutability,
                ident,
                ty,
                assignment,
            },
            var_span,
        )
    }
}

impl Parsable for Spanned<Statement> {
    async fn parse(cursor: &mut super::parse::Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        Ok(Statement::parse(cursor, reporter).await)
    }

    fn description(&self) -> &'static str {
        "statement"
    }
}

#[derive(Debug, Clone)]
pub struct ForLoop {
    header: Parenthesized<ForHeader>,
    content: Spanned<Statement>,
}

#[derive(Debug, Clone)]
pub struct ForHeader {
    init: Spanned<Statement>,
    check: Spanned<Expr>,
    post: Spanned<Statement>,
}

impl Parsable for ForHeader {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let init = Statement::parse(cursor, reporter).await;
        cursor.expect_semicolon(reporter).await;

        let check = Expr::parse_assignment(cursor, reporter).await;
        cursor.expect_semicolon(reporter).await;

        let post = Statement::parse(cursor, reporter).await;

        Ok(ForHeader { init, check, post })
    }

    fn description(&self) -> &'static str {
        "for loop header"
    }
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    check: Spanned<Expr>,
    contents: Spanned<Statement>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Void,
    Immediate(i128),
    Boolean(bool),
    Str(AsciiStr),
    Reference(Path),
    Call(Box<Spanned<Expr>>, Punctuated<Spanned<Expr>, Token![,]>),
    Tuple(Vec<Spanned<Expr>>),
    Array(Punctuated<Spanned<Expr>, Token![,]>),
    BinaryOp(Box<BinOp>),
    UnaryOp(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    As(Box<Spanned<Expr>>, Spanned<Type>),
    Sizeof(Spanned<Type>),
    Err,
}

impl Expr {
    //-------- Parsing --------//

    async fn parse_tuple(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Self> {
        let mut a = Expr::parse_assignment(cursor, reporter).await;

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Comma) => {
                    cursor.step();
                    let b = Expr::parse_assignment(cursor, reporter).await;
                    if let Expr::Tuple(ref mut components) = a.inner_mut() {
                        components.push(b);
                    } else {
                        let span = a.span().to(b.span());

                        let mut components = Vec::with_capacity(2);
                        components.push(a);
                        components.push(b);

                        a = Spanned::new(Expr::Tuple(components), span);
                    }
                }
                _ => return a,
            }
        }

        return a;
    }

    async fn parse_assignment(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Self> {
        let mut a = Box::pin(Expr::parse_boolean(cursor, reporter)).await;

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Eq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Assign, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                Token::Punctuation(Punctuation::PlusEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::AddEq, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                Token::Punctuation(Punctuation::MinusEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::SubEq, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                Token::Punctuation(Punctuation::MulEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::MulEq, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                Token::Punctuation(Punctuation::DivEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::DivEq, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                Token::Punctuation(Punctuation::ModEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::ModEq, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                Token::Punctuation(Punctuation::AndEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::AndEq, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                Token::Punctuation(Punctuation::OrEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::OrEq, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                Token::Punctuation(Punctuation::XorEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::XorEq, tok.span()),
                        BinopParse::Boolean,
                    ).await;
                }
                _ => return a,
            }
        }

        return a;
    }

    async fn parse_boolean(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Self> {
        let mut a = Expr::parse_comparison(cursor, reporter).await;

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::AndAnd) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::And, tok.span()),
                        BinopParse::Comparison,
                    ).await;
                }
                Token::Punctuation(Punctuation::PipePipe) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Or, tok.span()),
                        BinopParse::Comparison,
                    ).await;
                }
                _ => return a,
            }
        }

        return a;
    }

    async fn parse_comparison(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Self> {
        let mut a = Expr::parse_bitwise(cursor, reporter).await;

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::EqEq) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Equality, tok.span()),
                        BinopParse::Bitwise,
                    ).await;
                }
                Token::Punctuation(Punctuation::NotEqual) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Inequality, tok.span()),
                        BinopParse::Bitwise,
                    ).await;
                }
                Token::Punctuation(Punctuation::Lt) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Less, tok.span()),
                        BinopParse::Bitwise,
                    ).await;
                }
                Token::Punctuation(Punctuation::Le) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::LessEqual, tok.span()),
                        BinopParse::Bitwise,
                    ).await;
                }
                Token::Punctuation(Punctuation::Gt) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Greater, tok.span()),
                        BinopParse::Bitwise,
                    ).await;
                }
                Token::Punctuation(Punctuation::Ge) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::GreaterEqual, tok.span()),
                        BinopParse::Bitwise,
                    ).await;
                }
                _ => return a,
            }
        }

        return a;
    }

    async fn parse_bitwise(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Self> {
        let mut a = Expr::parse_expression(cursor, reporter).await;

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::And) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::BitAnd, tok.span()),
                        BinopParse::Expression,
                    ).await;
                }
                Token::Punctuation(Punctuation::Pipe) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::BitOr, tok.span()),
                        BinopParse::Expression,
                    ).await;
                }
                Token::Punctuation(Punctuation::Caret) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::BitXor, tok.span()),
                        BinopParse::Expression,
                    ).await;
                }
                _ => return a,
            }
        }

        return a;
    }

    async fn parse_expression(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Self> {
        let mut a = Expr::parse_terminal(cursor, reporter).await;

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Plus) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Add, tok.span()),
                        BinopParse::Terminal,
                    ).await;
                }
                Token::Punctuation(Punctuation::Minus) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Sub, tok.span()),
                        BinopParse::Terminal,
                    ).await;
                }
                _ => return a,
            }
        }

        return a;
    }

    async fn parse_terminal(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Self> {
        let mut a = Expr::parse_factor(cursor, reporter).await;

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Star) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Mul, tok.span()),
                        BinopParse::Factor,
                    ).await;
                }
                Token::Punctuation(Punctuation::Slash) => {
                    a = Expr::binop(
                        cursor,
                        reporter,
                        a,
                        Spanned::new(BinaryOp::Div, tok.span()),
                        BinopParse::Factor,
                    ).await;
                }
                Token::Keyword(Keyword::As) => {
                    cursor.step();

                    let ty = Type::parse(cursor, reporter).await;
                    let as_span = a.span().to(ty.span());

                    a = Spanned::new(Expr::As(Box::new(a), ty), as_span);
                }
                Token::Delimeter(Delimeter::OpenParen) => {
                    cursor.step();

                    let mut params_inner = Vec::new();
                    let mut last_param = None;

                    while let Some(tok) = cursor.peek().cloned() {
                        match tok.inner() {
                            Token::Delimeter(Delimeter::CloseParen) => break,
                            Token::Punctuation(Punctuation::Comma) => match last_param.take() {
                                Some(param) => {
                                    params_inner.push((param, Token![,]));
                                    cursor.step();
                                }
                                None => {
                                    reporter.report(error!(
                                        tok.span(),
                                        "unexpected duplicate seperator"
                                    )).await;

                                    cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                                    cursor.step();

                                    return Spanned::new(Expr::Err, tok.span());
                                }
                            },
                            // We use `parse_assignment` here instead of `parse` since `parse`
                            // just wraps the result of `parse_assignment` in a `Result`
                            _ => last_param = Some(Expr::parse_assignment(cursor, reporter).await),
                        }
                    }

                    let params = Punctuated::new(params_inner, last_param);

                    let close: Spanned<Token![")"]> = match cursor.parse(reporter).await {
                        Ok(close) => close,
                        Err(_) => {
                            cursor.step_back();

                            reporter.report(error!(
                                tok.span().clone(),
                                "unmatched opening parenthesis"
                            )).await;
                            return Spanned::new(Expr::Err, tok.span());
                        }
                    };

                    let call_span = a.span().to(close.span());
                    a = Spanned::new(Expr::Call(Box::new(a), params), call_span);
                }
                Token::Delimeter(Delimeter::OpenBracket) => {
                    cursor.step();

                    let idx = Expr::parse_assignment(cursor, reporter).await;

                    if cursor.check(&Token::Delimeter(Delimeter::CloseBracket)) {
                        let close_span = cursor.next().unwrap().span();
                        let expr_span = a.span().to(close_span);
                        let idx_span = tok.span().to(close_span);

                        a = Spanned::new(
                            Expr::BinaryOp(BinOp::boxed(
                                a,
                                idx,
                                Spanned::new(BinaryOp::Index, idx_span),
                            )),
                            expr_span,
                        )
                    }
                }
                _ => return a,
            }
        }

        return a;
    }

    async fn parse_factor(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Self> {
        let (tok, span) = match cursor.next() {
            Some(next) => next.deconstruct(),
            None => {
                reporter.report(error!(
                    cursor.eof(),
                    "expected expression, found `EOF`",
                )).await;

                return Spanned::new(Expr::Err, cursor.eof());
            }
        };

        match tok {
            //------ Literals ------//
            Token::Immediate(i) => Spanned::new(Expr::Immediate(i), span),
            Token::Boolean(b) => Spanned::new(Expr::Boolean(b), span),
            Token::String(string) => Spanned::new(Expr::Str(string), span),
            Token::Primitive(Primitive::Void) => Spanned::new(Expr::Void, span),
            Token::Ident(_) => {
                cursor.step_back();

                let (path, span) = match cursor.parse::<Spanned<Path>>(reporter).await {
                    Ok(path) => path.deconstruct(),
                    Err(err) => return Spanned::new(Expr::Err, span),
                };

                Spanned::new(Expr::Reference(path), span)
            }
            Token::Macro(Macro::Sizeof) => {
                let _: Token!["("] = inline_unwrap!(Expr, cursor, Result = cursor.parse(reporter).await);
                let ty = Type::parse(cursor, reporter).await;
                let _: Token![")"] = inline_unwrap!(Expr, cursor, Result = cursor.parse(reporter).await);

                let span = span.to(ty.span());
                Spanned::new(Expr::Sizeof(ty), span)
            }
            Token::Delimeter(Delimeter::OpenParen) => {
                let a = Expr::parse_tuple(cursor, reporter).await;

                match cursor.next().map(Spanned::into_inner) {
                    Some(Token::Delimeter(Delimeter::CloseParen)) => a,
                    _ => {
                        cursor.step_back();

                        reporter.report(error!(
                            span.clone(),
                            "unmatched opening parenthesis",
                        )).await;

                        Spanned::new(Expr::Err, span)
                    }
                }
            }
            Token::Delimeter(Delimeter::OpenBracket) => {
                let mut contents_inner = Vec::new();
                let mut last_expr = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Delimeter(Delimeter::CloseBracket) => break,
                        Token::Punctuation(Punctuation::Comma) => match last_expr.take() {
                            Some(last) => {
                                contents_inner.push((last, Token![,]));
                                cursor.step();
                            }
                            None => {
                                reporter.report(error!(
                                    tok.span().clone(),
                                    "unexpected duplicate seperator",
                                )).await;

                                return Spanned::new(Expr::Err, tok.span());
                            }
                        },
                        _ => last_expr = Some(Expr::parse_assignment(cursor, reporter).await),
                    }
                }

                let contents = Punctuated::new(contents_inner, last_expr);

                let close: Spanned<Token!["]"]> = match cursor.parse(reporter).await {
                    Ok(close) => close,
                    Err(_) => {
                        reporter
                            .report(error!(span.clone(), "unmatched opening bracket")).await;

                        return Spanned::new(Expr::Err, span);
                    }
                };

                let arr_span = span.to(close.span());
                Spanned::new(Expr::Array(contents), arr_span)
            }
            //------- Unary -------//
            Token::Punctuation(Punctuation::Not) => {
                Expr::unop(cursor, reporter, Spanned::new(UnaryOp::Not, span)).await
            }
            Token::Punctuation(Punctuation::Minus) => {
                Expr::unop(cursor, reporter, Spanned::new(UnaryOp::Negative, span)).await
            }
            Token::Punctuation(Punctuation::Star) => {
                Expr::unop(cursor, reporter, Spanned::new(UnaryOp::Deref, span)).await
            }
            Token::Punctuation(Punctuation::And) => {
                let (mutability, op_span) = if cursor.check(&Token::Keyword(Keyword::Mut)) {
                    (
                        Mutability::Mutable,
                        span.to(cursor.next().unwrap().span()),
                    )
                } else {
                    (Mutability::Immutable, span)
                };

                let expr = Box::pin(Expr::parse_factor(cursor, reporter)).await;
                let expr_span = op_span.to(expr.span());

                Spanned::new(
                    Expr::UnaryOp(
                        Spanned::new(UnaryOp::Ref(mutability), op_span),
                        Box::new(expr),
                    ),
                    expr_span,
                )
            }
            //----- Fallbacks -----//
            Token::Punctuation(Punctuation::Semicolon) => {
                cursor.step_back();

                reporter.report(error!(
                    span.clone(),
                    "expected expression, found {}",
                    tok.description(),
                )).await;

                Spanned::new(Expr::Err, span)
            }
            _ => {
                reporter.report(error!(
                    span.clone(),
                    "expected expression, found {}",
                    tok.description(),
                )).await;

                Spanned::new(Expr::Err, span)
            }
        }
    }

    //------- Utilities -------//

    async fn binop(
        cursor: &mut Cursor<'_>,
        reporter: &TerminalReporter<Stdout>,
        a: Spanned<Expr>,
        op: Spanned<BinaryOp>,
        parse: BinopParse,
    ) -> Spanned<Expr> {
        cursor.step();

        let b = match parse {
            BinopParse::Boolean => Box::pin(Expr::parse_boolean(cursor, reporter)).await,
            BinopParse::Factor => Box::pin(Expr::parse_factor(cursor, reporter)).await,
            BinopParse::Terminal => Box::pin(Expr::parse_terminal(cursor, reporter)).await,
            BinopParse::Comparison => Box::pin(Expr::parse_comparison(cursor, reporter)).await,
            BinopParse::Expression => Box::pin(Expr::parse_expression(cursor, reporter)).await,
            BinopParse::Bitwise => Box::pin(Expr::parse_bitwise(cursor, reporter)).await,
        };
        let expr_span = a.span().to(b.span());

        Spanned::new(Expr::BinaryOp(BinOp::boxed(a, b, op)), expr_span)
    }

    async fn unop(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>, op: Spanned<UnaryOp>) -> Spanned<Expr> {
        let expr = Box::pin(Expr::parse_factor(cursor, reporter)).await;
        let expr_span = op.span().to(expr.span());

        Spanned::new(Expr::UnaryOp(op, Box::new(expr)), expr_span)
    }
}

impl Parsable for Spanned<Expr> {
    #[inline]
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        Ok(Expr::parse_assignment(cursor, reporter).await)
    }

    fn description(&self) -> &'static str {
        "expression"
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinopParse {
    Boolean,
    Terminal,
    Factor,
    Comparison,
    Expression,
    Bitwise,
}

#[derive(Debug, Clone)]
pub struct Path {
    start: Spanned<PathSegment>,
    segments: Vec<Spanned<Ident>>,
}

impl Path {
    pub fn start(&self) -> &Spanned<PathSegment> {
        &self.start
    }

    pub fn end(&self) -> Spanned<PathSegment> {
        match self.segments.last() {
            Some(last) => {
                let ident = last.inner();
                let span = last.span().clone();
                Spanned::new(PathSegment::Ident(*ident), span)
            }
            None => self.start.clone(),
        }
    }

    pub fn segments(&self) -> &[Spanned<Ident>] {
        return self.segments.as_slice();
    }

    pub fn into_segments(self) -> Vec<Spanned<Ident>> {
        self.segments
    }

    pub fn end_segment(&self) -> Option<Spanned<Ident>> {
        self.segments.last().cloned()
    }
}

impl Parsable for Spanned<Path> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let start: Spanned<PathSegment> = cursor.parse(reporter).await?;
        let mut segments: Vec<Spanned<Ident>> = Vec::new();

        while !cursor.at_end() {
            if cursor.check(&Token::Punctuation(Punctuation::DoubleColon)) {
                cursor.step();
            } else {
                break;
            }

            let peek = cursor.peek();

            match peek.map(Spanned::inner) {
                Some(Token::Ident(id)) => {
                    let span = peek.unwrap().span().clone();
                    let symbol = *id;

                    cursor.step();

                    segments.push(Spanned::new(Ident { symbol }, span));
                }
                Some(tok) => {
                    reporter.report(error!(
                        peek.unwrap().span().clone(),
                        "expected identifier, found {}",
                        tok.description()
                    )).await;
                    return Err(());
                }
                None => {
                    reporter.report(error!(
                        cursor.eof(),
                        "expected identifier, found `EOF`"
                    )).await;
                    return Err(());
                }
            }
        }

        let span = match segments.last() {
            Some(last) => start.span().to(last.span()),
            None => start.span().clone(),
        };

        Ok(Spanned::new(Path { start, segments }, span))
    }

    fn description(&self) -> &'static str {
        "path"
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathSegment {
    Ident(Ident),
    Super,
    Root,
}

impl Parsable for Spanned<PathSegment> {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Result<Self, ()> {
        let peek = cursor.peek();

        let start = match peek.map(Spanned::inner) {
            Some(Token::Ident(id)) => {
                let ident = Ident { symbol: *id };
                Spanned::new(PathSegment::Ident(ident), peek.unwrap().span().clone())
            }
            Some(Token::Keyword(Keyword::Super)) => {
                Spanned::new(PathSegment::Super, peek.unwrap().span().clone())
            }
            Some(Token::Keyword(Keyword::Root)) => {
                Spanned::new(PathSegment::Root, peek.unwrap().span().clone())
            }
            Some(tok) => {
                reporter.report(error!(
                    peek.unwrap().span().clone(),
                    "expected path, found {}",
                    tok.description()
                )).await;
                return Err(());
            }
            None => {
                reporter.report(error!(
                    cursor.eof(),
                    "expected path, found `EOF`"
                )).await;
                return Err(());
            }
        };

        cursor.step();

        Ok(start)
    }

    fn description(&self) -> &'static str {
        "path start"
    }
}

#[derive(Debug, Clone)]
pub struct BinOp {
    lhs: Spanned<Expr>,
    op: Spanned<BinaryOp>,
    rhs: Spanned<Expr>,
}

impl BinOp {
    fn boxed(lhs: Spanned<Expr>, rhs: Spanned<Expr>, op: Spanned<BinaryOp>) -> Box<Self> {
        Box::new(BinOp { lhs, op, rhs })
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    AndEq,
    OrEq,
    XorEq,
    Equality,
    Inequality,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Index,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negative,
    Not,
    Deref,
    Ref(Mutability),
}

#[derive(Debug, Clone)]
pub enum Type {
    U8,
    U16,
    U24,
    U32,
    U64,
    I8,
    I16,
    I24,
    I32,
    I64,
    Bool,
    Void,
    Pointer {
        mutability: Mutability,
        ty: Box<Spanned<Type>>,
    },
    Tuple(Vec<Spanned<Type>>),
    Array(Box<Spanned<Type>>),
    Fn {
        parameters: Punctuated<Spanned<Type>, Token![,]>,
        return_type: Box<Spanned<Type>>,
    },
    Err,
}

impl Type {
    async fn parse(cursor: &mut Cursor<'_>, reporter: &TerminalReporter<Stdout>) -> Spanned<Type> {
        if let Some(tok) = cursor.next() {
            match tok.inner() {
                Token::Primitive(p) => Spanned::new(p.into(), tok.span()),
                Token::Punctuation(Punctuation::Star) => {
                    let start = tok.span();

                    let mutability = if cursor.check(&Token::Keyword(Keyword::Mut)) {
                        cursor.step();
                        Mutability::Mutable
                    } else {
                        Mutability::Immutable
                    };

                    let ty: Spanned<Type> = Box::pin(Type::parse(cursor, reporter)).await;
                    let span = start.to(ty.span());

                    Spanned::new(
                        Type::Pointer {
                            mutability,
                            ty: Box::new(ty),
                        },
                        span,
                    )
                }
                Token::Delimeter(Delimeter::OpenBracket) => {
                    let ty = Box::pin(Type::parse(cursor, reporter)).await;
                    let close: Spanned<Token!["]"]> =
                        inline_unwrap!(Type, cursor, Result = cursor.parse(reporter).await);

                    let span = tok.span().to(close.span());
                    Spanned::new(Type::Array(Box::new(ty)), span)
                }
                Token::Delimeter(Delimeter::OpenParen) => {
                    let start = tok.span();

                    let mut comma = true;
                    let mut types = Vec::new();

                    while !cursor.check(&Token::Delimeter(Delimeter::CloseParen))
                        && !cursor.at_end()
                    {
                        if !comma {
                            // We can unwrap here since we are already checking if the cursor has more tokens
                            let (next_tok, next_span) = cursor.next().unwrap().deconstruct();

                            reporter.report(error!(
                                next_span,
                                "expected `,`, found {}",
                                next_tok.description()
                            )).await;

                            seek!(
                                cursor,
                                Token::Delimeter(Delimeter::CloseParen)
                                    | Token::Punctuation(Punctuation::Comma)
                            );
                        } else {
                            types.push(Box::pin(Type::parse(cursor, reporter)).await);
                            comma = if cursor.check(&Token::Punctuation(Punctuation::Comma)) {
                                cursor.step();
                                true
                            } else {
                                false
                            };
                        }
                    }

                    let close: Spanned<Token![")"]> = match cursor.parse(reporter).await {
                        Ok(close) => close,
                        Err(err) => return Spanned::new(Type::Err, cursor.eof()),
                    };

                    Spanned::new(Type::Tuple(types), start.to(close.span()))
                }
                Token::Keyword(Keyword::Fn) => {
                    let _: Token!["("] = inline_unwrap!(Type, cursor, Result = cursor.parse(reporter).await);

                    let mut params_inner = Vec::new();
                    let mut last_param = None;

                    while let Some(tok) = cursor.next() {
                        match tok.inner() {
                            Token::Delimeter(Delimeter::CloseParen) => break,
                            Token::Punctuation(Punctuation::Comma) => match last_param.take() {
                                Some(param) => params_inner.push((param, Token![,])),
                                None => {
                                    reporter.report(error!(
                                        tok.span().clone(),
                                        "unexpected duplicate seperator"
                                    )).await;

                                    cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                                    cursor.step();

                                    return Spanned::new(Type::Err, tok.span());
                                }
                            },
                            _ => {
                                cursor.step_back();
                                last_param = Some(Box::pin(Type::parse(cursor, reporter)).await)
                            }
                        }
                    }

                    let parameters = Punctuated::new(params_inner, last_param);
                    let _: Token![:] = inline_unwrap!(Type, cursor, Result = cursor.parse(reporter).await);
                    let return_type = Box::pin(Type::parse(cursor, reporter)).await;

                    let ty_span = tok.span().to(return_type.span());
                    Spanned::new(
                        Type::Fn {
                            parameters,
                            return_type: Box::new(return_type),
                        },
                        ty_span,
                    )
                }
                other => {
                    cursor.step_back();

                    reporter.report(error!(
                        tok.span().clone(),
                        "expected type, found {}",
                        other.description(),
                    )).await;
                    Spanned::new(Type::Err, tok.span())
                }
            }
        } else {
            reporter.report(error!(
                cursor.eof(),
                "expected type, found `EOF`"
            )).await;
            Spanned::new(Type::Err, cursor.eof())
        }
    }
}

impl From<&Primitive> for Type {
    fn from(value: &Primitive) -> Self {
        match value {
            Primitive::Void => Type::Void,
            Primitive::Bool => Type::Bool,
            Primitive::U8 => Type::U8,
            Primitive::U16 => Type::U16,
            Primitive::U24 => Type::U24,
            Primitive::U32 => Type::U32,
            Primitive::U64 => Type::U64,
            Primitive::I8 => Type::I8,
            Primitive::I16 => Type::I16,
            Primitive::I24 => Type::I24,
            Primitive::I32 => Type::I32,
            Primitive::I64 => Type::I64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Immutable,
    Mutable,
}
