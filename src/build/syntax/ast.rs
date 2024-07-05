use crate::{build::ascii::AsciiStr, seek, span::Spanned, spanned_error, Token};

use super::{
    lex::{Delimeter, Keyword, Primitive, Punctuation, Token},
    parse::{Cursor, Parenthesized, Parsable, Punctuated},
    token::Ident,
};

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
    Asm(Spanned<String>),
    Var {
        mutability: Mutability,
        ident: Spanned<Ident>,
        ty: Spanned<Type>,
        assignment: Spanned<Expr>,
    },
    Err,
}

macro_rules! inline_unwrap {
    ($cursor:expr, Result = $expr:expr, $enum:ident) => {
        match $expr {
            Ok(ok) => ok,
            Err(err) => {
                $cursor.reporter().report_sync(err);
                return Spanned::new($enum::Err, $cursor.eof_span());
            }
        }
    };
    ($cursor:expr, Option = $expr:expr, $err:expr, $enum:ident) => {
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
            Statement::Expr(_) | Statement::Return(_) | Statement::Break | Statement::Continue | Statement::Asm(_) | Statement::Var{..} => true,
            _ => false,
        }
    }

    fn parse(cursor: &mut Cursor) -> Spanned<Statement> {
        let (tok, span) = inline_unwrap!(cursor, Option = cursor.next(), spanned_error!(cursor.eof_span(), "expected statemenet, found `EOF`"), Statement)
            .deconstruct();

        match tok {
            Token::Keyword(Keyword::If) => {
                let condition = inline_unwrap!(cursor, Result = cursor.parse(), Statement);

                let content: Spanned<Statement> = inline_unwrap!(cursor, Result = cursor.parse(), Statement);

                let (else_clause, if_span) = if cursor.check(&Token::Keyword(Keyword::Else)) {
                    let else_clause: Spanned<Statement> = inline_unwrap!(cursor, Result = cursor.parse(), Statement);
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
                let header = inline_unwrap!(cursor, Result = cursor.parse(), Statement);
                let content = Statement::parse(cursor);

                let for_span = span.to(content.span());
                let for_loop = ForLoop{header, content};

                Spanned::new(Statement::For(Box::new(for_loop)), for_span)
            }
            Token::Keyword(Keyword::While) => {
                let check = inline_unwrap!(cursor, Result = cursor.parse(), Statement);
                let contents = Statement::parse(cursor);

                let while_span = span.to(contents.span());
                let while_loop = WhileLoop {check, contents};

                Spanned::new(Statement::While(Box::new(while_loop)), while_span)
            }
            Token::Keyword(Keyword::Break) => Spanned::new(Statement::Break, span),
            Token::Keyword(Keyword::Continue) => Spanned::new(Statement::Continue, span),
            Token::Keyword(Keyword::Return) => {
                let value = Expr::parse_assignment(cursor);

                let return_span = span.to(value.span());
                Spanned::new(Statement::Return(value), return_span)
            }
            _ => {
                cursor.step_back();

                let (expr, span) = Expr::parse_assignment(cursor).deconstruct();
                Spanned::new(Statement::Expr(expr), span)
            }
        }

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
        
        Ok(ForHeader{init, check, post})
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
                            Some(last) => contents_inner.push((last, Token![,])),
                            None => {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "unexpected duplicate seperator",
                                ));

                                return Spanned::new(Expr::Err, tok.into_span());
                            }
                        }
                        _ => last_expr = Some(Expr::parse_assignment(cursor)),
                    }
                }

                let contents = Punctuated::new(contents_inner, last_expr);

                let close: Spanned<Token!["]"]> = match cursor.parse() {
                    Ok(close) => close,
                    Err(_) => {
                        cursor.reporter().report_sync(spanned_error!(
                            span.clone(), "unmatched opening bracket"
                        ));

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
    Err,
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

impl Parsable for Spanned<Type> {
    fn parse(cursor: &mut super::parse::Cursor) -> Result<Self, crate::diagnostic::Diagnostic> {
        let peek = cursor.peek();

        match peek.map(|spanned| spanned.inner()) {
            Some(Token::Primitive(p)) => Ok(Spanned::new(p.into(), peek.unwrap().span().clone())),
            Some(Token::Punctuation(Punctuation::Star)) => {
                let start = peek.unwrap().span().clone();
                cursor.step();

                let mutability = if cursor.check(&Token::Keyword(Keyword::Mut)) {
                    cursor.step();
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                };

                let ty: Spanned<Type> = cursor.parse()?;
                let span = start.to(ty.span());

                Ok(Spanned::new(
                    Type::Pointer {
                        mutability,
                        ty: Box::new(ty),
                    },
                    span,
                ))
            }
            Some(Token::Delimeter(Delimeter::OpenParen)) => {
                let start = peek.unwrap().span().clone();

                let mut comma = true;
                let mut types = Vec::new();

                while !cursor.check(&Token::Delimeter(Delimeter::CloseParen)) && !cursor.at_end() {
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
                        types.push(cursor.parse()?);
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
                        return Ok(Spanned::new(Type::Err, cursor.eof_span()));
                    }
                };

                Ok(Spanned::new(Type::Tuple(types), start.to(close.span())))
            }
            Some(tok) => {
                cursor.reporter().report_sync(spanned_error!(
                    peek.unwrap().span().clone(),
                    "expected type, found {}",
                    tok.description()
                ));
                Ok(Spanned::new(Type::Err, peek.unwrap().span().clone()))
            }
            None => {
                cursor.reporter().report_sync(spanned_error!(
                    cursor.eof_span(),
                    "expected type, found `EOF`"
                ));
                Ok(Spanned::new(Type::Err, cursor.eof_span()))
            }
        }
    }

    fn description(&self) -> &'static str {
        "type"
    }
}

#[derive(Debug, Clone)]
pub enum Mutability {
    Immutable,
    Mutable,
}
