use crate::{
    build::ascii::AsciiStr,
    seek,
    span::{Span, Spanned},
    spanned_debug, spanned_error, Token,
};

use super::{
    info::LibSrc,
    lex::{Delimeter, Keyword, Macro, Primitive, Punctuation, Token},
    parse::{Cursor, Parenthesized, Parsable, Punctuated},
    token::{Ident, LitString},
};

#[derive(Debug, Clone)]
pub struct Const {
    ident: Spanned<Ident>,
    value: Spanned<Expr>,
}

impl Parsable for Spanned<Const> {
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let keyword: Spanned<Token![const]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;
        let _: Token![=] = cursor.parse()?;
        let value = Expr::parse_assignment(cursor);

        let const_span = keyword.span().to(value.span());
        Ok(Spanned::new(Const { ident, value }, const_span))
    }

    fn description(&self) -> &'static str {
        "constant"
    }
}

#[derive(Debug, Clone)]
pub struct Static {
    ident: Spanned<Ident>,
    ty: Spanned<Type>,
    value: Spanned<Expr>,
}

impl Parsable for Spanned<Static> {
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let keyword: Spanned<Token![static]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;

        let _: Token![:] = cursor.parse()?;
        let ty = Type::parse(cursor);

        let _: Token![=] = cursor.parse()?;
        let value = Expr::parse_assignment(cursor);

        let static_span = keyword.span().to(value.span());
        Ok(Spanned::new(Static { ident, ty, value }, static_span))
    }

    fn description(&self) -> &'static str {
        "static variable"
    }
}

#[derive(Debug, Clone)]
pub struct Progmem {
    ident: Spanned<Ident>,
    ty: Spanned<Type>,
    value: Spanned<Expr>,
}

impl Parsable for Spanned<Progmem> {
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let keyword: Spanned<Token![progmem]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;

        let _: Token![:] = cursor.parse()?;
        let ty = Type::parse(cursor);

        let _: Token![=] = cursor.parse()?;
        let value = Expr::parse_assignment(cursor);

        let static_span = keyword.span().to(value.span());
        Ok(Spanned::new(Progmem { ident, ty, value }, static_span))
    }

    fn description(&self) -> &'static str {
        "program memory variable"
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    ident: Spanned<Ident>,
    parameters: Punctuated<Spanned<Parameter>, Token![,]>,
    return_type: Spanned<Type>,
    body: Spanned<Statement>,
}

impl Parsable for Spanned<Function> {
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let keyword: Spanned<Token![fn]> = cursor.parse()?;
        let ident = cursor.parse()?;
        let _: Token!["("] = cursor.parse()?;

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
                        cursor.reporter().report_sync(spanned_error!(
                            tok.span().clone(),
                            "unexpected duplicate seperator"
                        ));
                        cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                        break;
                    }
                },
                _ => {
                    let param = match cursor.parse() {
                        Ok(param) => param,
                        Err(err) => {
                            cursor.reporter().report_sync(err);
                            cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                            break;
                        }
                    };

                    last_param = Some(param);
                }
            }
        }

        let parameters = Punctuated::new(params_inner, last_param);

        let close_paren: Spanned<Token![")"]> = cursor.parse()?;

        let return_type = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();
            Type::parse(cursor)
        } else {
            cursor.reporter().report_sync(spanned_error!(
                close_paren.span().clone(),
                "missing return type"
            ));
            Spanned::new(Type::Err, close_paren.into_span())
        };

        let body = Statement::parse(cursor);

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
    ident: Spanned<Ident>,
    ty: Spanned<Type>,
}

impl Parsable for Spanned<Parameter> {
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let ident: Spanned<Ident> = cursor.parse()?;
        let _: Token![:] = cursor.parse()?;
        let ty = Type::parse(cursor);

        let param_span = ident.span().to(ty.span());
        Ok(Spanned::new(Parameter { ident, ty }, param_span))
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
    Return(Spanned<Expr>),
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
            Err(err) => {
                $cursor.reporter().report_sync(err);
                return Spanned::new($enum::Err, $cursor.eof_span());
            }
        }
    };
    ($enum:ident, $cursor:expr, Option = $expr:expr, $err:expr) => {
        match $expr {
            Some(some) => some,
            None => {
                $cursor.reporter().report_sync($err);
                return Spanned::new($enum::Err, $cursor.eof_span());
            }
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

    pub fn parse(cursor: &mut Cursor) -> Spanned<Statement> {
        let (tok, span) = inline_unwrap!(
            Statement,
            cursor,
            Option = cursor.next(),
            spanned_error!(cursor.eof_span(), "expected statemenet, found `EOF`")
        )
        .deconstruct();

        match tok {
            Token::Keyword(Keyword::If) => {
                let condition = inline_unwrap!(Statement, cursor, Result = cursor.parse());

                let content: Spanned<Statement> =
                    inline_unwrap!(Statement, cursor, Result = cursor.parse());

                let (else_clause, if_span) = if cursor.check(&Token::Keyword(Keyword::Else)) {
                    let else_clause: Spanned<Statement> =
                        inline_unwrap!(Statement, cursor, Result = cursor.parse());
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
                let header = inline_unwrap!(Statement, cursor, Result = cursor.parse());
                let content = Statement::parse(cursor);

                let for_span = span.to(content.span());
                let for_loop = ForLoop { header, content };

                Spanned::new(Statement::For(Box::new(for_loop)), for_span)
            }
            Token::Keyword(Keyword::While) => {
                let check = inline_unwrap!(Statement, cursor, Result = cursor.parse());
                let contents = Statement::parse(cursor);

                let while_span = span.to(contents.span());
                let while_loop = WhileLoop { check, contents };

                Spanned::new(Statement::While(Box::new(while_loop)), while_span)
            }
            Token::Keyword(Keyword::Break) => Spanned::new(Statement::Break, span),
            Token::Keyword(Keyword::Continue) => Spanned::new(Statement::Continue, span),
            Token::Keyword(Keyword::Return) => {
                let value = Expr::parse_assignment(cursor);

                let return_span = span.to(value.span());
                Spanned::new(Statement::Return(value), return_span)
            }
            Token::Delimeter(Delimeter::OpenBrace) => {
                let mut statements = Vec::new();

                while let Some(tok) = cursor.peek() {
                    match tok.inner() {
                        Token::Punctuation(Punctuation::Semicolon) => cursor.step(),
                        Token::Delimeter(Delimeter::CloseBrace) => break,
                        _ => {
                            let statement = Statement::parse(cursor);
                            if statement.requires_semicolon() {
                                cursor.expect_semicolon();
                            }

                            statements.push(statement);
                        }
                    }
                }

                let close: Spanned<Token!["}"]> = match cursor.parse() {
                    Ok(close) => close,
                    Err(_) => {
                        cursor.step_back();

                        cursor
                            .reporter()
                            .report_sync(spanned_error!(span.clone(), "unmatched opening brace"));
                        return Spanned::new(Statement::Err, span);
                    }
                };

                let block_span = span.to(close.span());
                Spanned::new(Statement::Block(statements), block_span)
            }
            Token::Macro(Macro::Asm) => {
                let string: Parenthesized<Spanned<LitString>> =
                    inline_unwrap!(Statement, cursor, Result = cursor.parse());
                let asm_span = span.to(string.span());

                Spanned::new(Statement::Asm(string), asm_span)
            }
            Token::Keyword(Keyword::Let) => {
                Statement::variable(cursor, span, Mutability::Immutable)
            }
            Token::Keyword(Keyword::Mut) => Statement::variable(cursor, span, Mutability::Mutable),
            _ => {
                cursor.step_back();

                let (expr, span) = Expr::parse_assignment(cursor).deconstruct();
                Spanned::new(Statement::Expr(expr), span)
            }
        }
    }

    fn variable(
        cursor: &mut Cursor,
        keyword_span: Span,
        mutability: Mutability,
    ) -> Spanned<Statement> {
        let ident = inline_unwrap!(Statement, cursor, Result = cursor.parse());

        let ty = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();
            Some(Type::parse(cursor))
        } else {
            None
        };

        let _: Token![=] = inline_unwrap!(Statement, cursor, Result = cursor.parse());
        let assignment = Expr::parse_assignment(cursor);

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
    fn parse(cursor: &mut super::parse::Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        Ok(Statement::parse(cursor))
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
    check: Spanned<Statement>,
    post: Spanned<Statement>,
}

impl Parsable for ForHeader {
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let init = Statement::parse(cursor);
        cursor.expect_semicolon();

        let check = Statement::parse(cursor);
        cursor.expect_semicolon();

        let post = Statement::parse(cursor);

        Ok(ForHeader { init, check, post })
    }

    fn description(&self) -> &'static str {
        "for loop header"
    }
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    check: Spanned<Statement>,
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

    fn parse_tuple(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_assignment(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Comma) => {
                    cursor.step();
                    let b = Expr::parse_assignment(cursor);
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

    fn parse_assignment(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_boolean(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Eq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Assign, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                Token::Punctuation(Punctuation::PlusEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::AddEq, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                Token::Punctuation(Punctuation::MinusEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::SubEq, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                Token::Punctuation(Punctuation::MulEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::MulEq, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                Token::Punctuation(Punctuation::DivEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::DivEq, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                Token::Punctuation(Punctuation::ModEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::ModEq, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                Token::Punctuation(Punctuation::AndEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::AndEq, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                Token::Punctuation(Punctuation::OrEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::OrEq, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                Token::Punctuation(Punctuation::XorEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::XorEq, tok.into_span()),
                        Expr::parse_boolean,
                    );
                }
                _ => return a,
            }
        }

        return a;
    }

    fn parse_boolean(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_comparison(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::AndAnd) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::And, tok.into_span()),
                        Expr::parse_comparison,
                    );
                }
                Token::Punctuation(Punctuation::PipePipe) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Or, tok.into_span()),
                        Expr::parse_comparison,
                    );
                }
                _ => return a,
            }
        }

        return a;
    }

    fn parse_comparison(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_bitwise(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::EqEq) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Equality, tok.into_span()),
                        Expr::parse_bitwise,
                    );
                }
                Token::Punctuation(Punctuation::NotEqual) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Inequality, tok.into_span()),
                        Expr::parse_bitwise,
                    );
                }
                Token::Punctuation(Punctuation::Lt) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Less, tok.into_span()),
                        Expr::parse_bitwise,
                    );
                }
                Token::Punctuation(Punctuation::Le) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::LessEqual, tok.into_span()),
                        Expr::parse_bitwise,
                    );
                }
                Token::Punctuation(Punctuation::Gt) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Greater, tok.into_span()),
                        Expr::parse_bitwise,
                    );
                }
                Token::Punctuation(Punctuation::Ge) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::GreaterEqual, tok.into_span()),
                        Expr::parse_bitwise,
                    );
                }
                _ => return a,
            }
        }

        return a;
    }

    fn parse_bitwise(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_expression(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::And) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::BitAnd, tok.into_span()),
                        Expr::parse_expression,
                    );
                }
                Token::Punctuation(Punctuation::Pipe) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::BitOr, tok.into_span()),
                        Expr::parse_expression,
                    );
                }
                Token::Punctuation(Punctuation::Caret) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::BitXor, tok.into_span()),
                        Expr::parse_expression,
                    );
                }
                _ => return a,
            }
        }

        return a;
    }

    fn parse_expression(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_terminal(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Plus) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Add, tok.into_span()),
                        Expr::parse_terminal,
                    );
                }
                Token::Punctuation(Punctuation::Minus) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Sub, tok.into_span()),
                        Expr::parse_terminal,
                    );
                }
                _ => return a,
            }
        }

        return a;
    }

    fn parse_terminal(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_factor(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Star) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Mul, tok.into_span()),
                        Expr::parse_factor,
                    );
                }
                Token::Punctuation(Punctuation::Slash) => {
                    a = Expr::binop(
                        cursor,
                        a,
                        Spanned::new(BinaryOp::Div, tok.into_span()),
                        Expr::parse_factor,
                    );
                }
                Token::Keyword(Keyword::As) => {
                    cursor.step();

                    let ty = Type::parse(cursor);
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
                                Some(param) => params_inner.push((param, Token![,])),
                                None => {
                                    cursor.reporter().report_sync(spanned_error!(
                                        tok.span().clone(),
                                        "unexpected duplicate seperator"
                                    ));

                                    cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                                    cursor.step();

                                    return Spanned::new(Expr::Err, tok.into_span());
                                }
                            },
                            // We use `parse_assignment` here instead of `parse` since `parse`
                            // just wraps the result of `parse_assignment` in a `Result`
                            _ => last_param = Some(Expr::parse_assignment(cursor)),
                        }
                    }

                    let params = Punctuated::new(params_inner, last_param);

                    let close: Spanned<Token![")"]> = match cursor.parse() {
                        Ok(close) => close,
                        Err(_) => {
                            cursor.step_back();

                            cursor.reporter().report_sync(spanned_error!(
                                tok.span().clone(),
                                "unmatched opening parenthesis"
                            ));
                            return Spanned::new(Expr::Err, tok.into_span());
                        }
                    };

                    let call_span = a.span().to(close.span());
                    a = Spanned::new(Expr::Call(Box::new(a), params), call_span);
                }
                Token::Delimeter(Delimeter::OpenBracket) => {
                    cursor.step();

                    let idx = Expr::parse_assignment(cursor);

                    if cursor.check(&Token::Delimeter(Delimeter::CloseBracket)) {
                        let close_span = cursor.next().unwrap().into_span();
                        let expr_span = a.span().to(&close_span);
                        let idx_span = tok.span().to(&close_span);

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

    fn parse_factor(cursor: &mut Cursor) -> Spanned<Self> {
        let (tok, span) = match cursor.next() {
            Some(next) => next.deconstruct(),
            None => {
                cursor.reporter().report_sync(spanned_error!(
                    cursor.eof_span(),
                    "expected expression, found `EOF`",
                ));

                return Spanned::new(Expr::Err, cursor.eof_span());
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

                let (path, span) = match cursor.parse::<Spanned<Path>>() {
                    Ok(path) => path.deconstruct(),
                    Err(err) => {
                        cursor.reporter().report_sync(err);
                        return Spanned::new(Expr::Err, span);
                    }
                };

                Spanned::new(Expr::Reference(path), span)
            }
            Token::Macro(Macro::Sizeof) => {
                let _: Token!["("] = inline_unwrap!(Expr, cursor, Result = cursor.parse());
                let ty = Type::parse(cursor);
                let _: Token![")"] = inline_unwrap!(Expr, cursor, Result = cursor.parse());

                let span = span.to(ty.span());
                Spanned::new(Expr::Sizeof(ty), span)
            }
            Token::Delimeter(Delimeter::OpenParen) => {
                let a = Expr::parse_tuple(cursor);

                match cursor.next().map(Spanned::into_inner) {
                    Some(Token::Delimeter(Delimeter::CloseParen)) => a,
                    _ => {
                        cursor.step_back();

                        cursor.reporter().report_sync(spanned_error!(
                            span.clone(),
                            "unmatched opening parenthesis",
                        ));

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
                            },
                            None => {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "unexpected duplicate seperator",
                                ));

                                return Spanned::new(Expr::Err, tok.into_span());
                            }
                        },
                        _ => last_expr = Some(Expr::parse_assignment(cursor)),
                    }
                }

                let contents = Punctuated::new(contents_inner, last_expr);

                let close: Spanned<Token!["]"]> = match cursor.parse() {
                    Ok(close) => close,
                    Err(_) => {
                        cursor
                            .reporter()
                            .report_sync(spanned_error!(span.clone(), "unmatched opening bracket"));

                        return Spanned::new(Expr::Err, span);
                    }
                };

                let arr_span = span.to(close.span());
                Spanned::new(Expr::Array(contents), arr_span)
            }
            //------- Unary -------//
            Token::Punctuation(Punctuation::Not) => {
                Expr::unop(cursor, Spanned::new(UnaryOp::Not, span))
            }
            Token::Punctuation(Punctuation::Minus) => {
                Expr::unop(cursor, Spanned::new(UnaryOp::Negative, span))
            }
            Token::Punctuation(Punctuation::Star) => {
                Expr::unop(cursor, Spanned::new(UnaryOp::Deref, span))
            }
            Token::Punctuation(Punctuation::And) => {
                let (mutability, op_span) = if cursor.check(&Token::Keyword(Keyword::Mut)) {
                    (
                        Mutability::Mutable,
                        span.to(&cursor.next().unwrap().into_span()),
                    )
                } else {
                    (Mutability::Immutable, span)
                };

                let expr = Expr::parse_factor(cursor);
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

                cursor.reporter().report_sync(spanned_error!(
                    span.clone(),
                    "expected expression, found {}",
                    tok.description(),
                ));

                Spanned::new(Expr::Err, span)
            }
            _ => {
                cursor.reporter().report_sync(spanned_error!(
                    span.clone(),
                    "expected expression, found {}",
                    tok.description(),
                ));

                Spanned::new(Expr::Err, span)
            }
        }
    }

    //------- Utilities -------//

    fn binop(
        cursor: &mut Cursor,
        a: Spanned<Expr>,
        op: Spanned<BinaryOp>,
        parse: fn(&mut Cursor) -> Spanned<Expr>,
    ) -> Spanned<Expr> {
        cursor.step();

        let b = parse(cursor);
        let expr_span = a.span().to(b.span());

        Spanned::new(Expr::BinaryOp(BinOp::boxed(a, b, op)), expr_span)
    }

    fn unop(cursor: &mut Cursor, op: Spanned<UnaryOp>) -> Spanned<Expr> {
        let expr = Expr::parse_factor(cursor);
        let expr_span = op.span().to(expr.span());

        Spanned::new(Expr::UnaryOp(op, Box::new(expr)), expr_span)
    }
}

impl Parsable for Spanned<Expr> {
    #[inline]
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        Ok(Expr::parse_assignment(cursor))
    }

    fn description(&self) -> &'static str {
        "expression"
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    start: Spanned<PathStart>,
    segments: Vec<Spanned<Ident>>,
}

impl Parsable for Spanned<Path> {
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let peek = cursor.peek();

        let start: Spanned<PathStart> = cursor.parse()?;

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
                    return Err(spanned_error!(
                        peek.unwrap().span().clone(),
                        "expected identifier, found {}",
                        tok.description()
                    ))
                }
                None => {
                    return Err(spanned_error!(
                        cursor.eof_span(),
                        "expected identifier, found `EOF`"
                    ))
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

#[derive(Debug, Clone)]
pub enum PathStart {
    Ident(Ident),
    Super,
    Root,
}

impl Parsable for Spanned<PathStart> {
    fn parse(cursor: &mut Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let peek = cursor.peek();

        let start = match peek.map(Spanned::inner) {
            Some(Token::Ident(id)) => {
                let ident = Ident { symbol: *id };
                Spanned::new(PathStart::Ident(ident), peek.unwrap().span().clone())
            }
            Some(Token::Keyword(Keyword::Super)) => {
                Spanned::new(PathStart::Super, peek.unwrap().span().clone())
            }
            Some(Token::Keyword(Keyword::Root)) => {
                Spanned::new(PathStart::Root, peek.unwrap().span().clone())
            }
            Some(tok) => {
                return Err(spanned_error!(
                    peek.unwrap().span().clone(),
                    "expected path, found {}",
                    tok.description()
                ))
            }
            None => {
                return Err(spanned_error!(
                    cursor.eof_span(),
                    "expected path, found `EOF`"
                ))
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
    Array (Box<Spanned<Type>>),
    Fn {
        parameters: Punctuated<Spanned<Type>, Token![,]>,
        return_type: Box<Spanned<Type>>,
    },
    Err,
}

impl Type {
    fn parse(cursor: &mut super::parse::Cursor) -> Spanned<Type> {
        if let Some(tok) = cursor.next() {
            match tok.inner() {
                Token::Primitive(p) => Spanned::new(p.into(), tok.into_span()),
                Token::Punctuation(Punctuation::Star) => {
                    let start = tok.into_span();

                    let mutability = if cursor.check(&Token::Keyword(Keyword::Mut)) {
                        cursor.step();
                        Mutability::Mutable
                    } else {
                        Mutability::Immutable
                    };

                    let ty: Spanned<Type> = Type::parse(cursor);
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
                    let ty = Type::parse(cursor);
                    let close: Spanned<Token!["]"]> = inline_unwrap!(Type, cursor, Result = cursor.parse());

                    let span = tok.span().to(close.span());
                    Spanned::new(Type::Array(Box::new(ty)), span)
                }
                Token::Delimeter(Delimeter::OpenParen) => {
                    let start = tok.into_span();

                    let mut comma = true;
                    let mut types = Vec::new();

                    while !cursor.check(&Token::Delimeter(Delimeter::CloseParen))
                        && !cursor.at_end()
                    {
                        if !comma {
                            // We can unwrap here since we are already checking if the cursor has more tokens
                            let (next_tok, next_span) = cursor.next().unwrap().deconstruct();

                            cursor.reporter().report_sync(spanned_error!(
                                next_span,
                                "expected `,`, found {}",
                                next_tok.description()
                            ));

                            seek!(
                                cursor,
                                Token::Delimeter(Delimeter::CloseParen)
                                    | Token::Punctuation(Punctuation::Comma)
                            );
                        } else {
                            types.push(Type::parse(cursor));
                            comma = if cursor.check(&Token::Punctuation(Punctuation::Comma)) {
                                cursor.step();
                                true
                            } else {
                                false
                            };
                        }
                    }

                    let close: Spanned<Token![")"]> = match cursor.parse() {
                        Ok(close) => close,
                        Err(err) => {
                            cursor.reporter().report_sync(err);
                            return Spanned::new(Type::Err, cursor.eof_span());
                        }
                    };

                    Spanned::new(Type::Tuple(types), start.to(close.span()))
                }
                Token::Keyword(Keyword::Fn) => {
                    let _: Token!["("] = inline_unwrap!(Type, cursor, Result = cursor.parse());

                    let mut params_inner = Vec::new();
                    let mut last_param = None;

                    while let Some(tok) = cursor.next() {
                        match tok.inner() {
                            Token::Delimeter(Delimeter::CloseParen) => break,
                            Token::Punctuation(Punctuation::Comma) => match last_param.take() {
                                Some(param) => params_inner.push((param, Token![,])),
                                None => {
                                    cursor.reporter().report_sync(spanned_error!(
                                        tok.span().clone(),
                                        "unexpected duplicate seperator"
                                    ));

                                    cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                                    cursor.step();

                                    return Spanned::new(Type::Err, tok.into_span());
                                }
                            },
                            _ => {
                                cursor.step_back();
                                last_param = Some(Type::parse(cursor))
                            }
                        }
                    }

                    let parameters = Punctuated::new(params_inner, last_param);
                    let _: Token![:] = inline_unwrap!(Type, cursor, Result = cursor.parse());
                    let return_type = Type::parse(cursor);

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

                    cursor.reporter().report_sync(spanned_error!(
                        tok.span().clone(),
                        "expected type, found {}",
                        other.description(),
                    ));
                    Spanned::new(Type::Err, tok.into_span())
                }
            }
        } else {
            cursor.reporter().report_sync(spanned_error!(
                cursor.eof_span(),
                "expected type, found `EOF`"
            ));
            Spanned::new(Type::Err, cursor.eof_span())
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
