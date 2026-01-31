use std::iter::Peekable;

use logos::Span;

use crate::ast::{BinOp, EnumVariant, Expr, Literal, MatchArm, Param, Pattern, Program, Spanned, Stmt, Type};
use crate::lexer::{Lexer, Token};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken {
        expected: Vec<String>,
        found: Option<Token>,
    },
    UnexpectedEof {
        expected: Vec<String>,
    },
    LexerError,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl ParseError {
    fn unexpected_token(expected: Vec<&str>, found: Option<Token>, span: Span) -> Self {
        ParseError {
            kind: ParseErrorKind::UnexpectedToken {
                expected: expected.into_iter().map(String::from).collect(),
                found,
            },
            span,
        }
    }

    fn unexpected_eof(expected: Vec<&str>, span: Span) -> Self {
        ParseError {
            kind: ParseErrorKind::UnexpectedEof {
                expected: expected.into_iter().map(String::from).collect(),
            },
            span,
        }
    }

    fn lexer_error(span: Span) -> Self {
        ParseError {
            kind: ParseErrorKind::LexerError,
            span,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParseErrorKind::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "unexpected token at {}..{}: expected one of {:?}, found {:?}",
                    self.span.start, self.span.end, expected, found
                )
            }
            ParseErrorKind::UnexpectedEof { expected } => {
                write!(
                    f,
                    "unexpected end of input at {}: expected one of {:?}",
                    self.span.start, expected
                )
            }
            ParseErrorKind::LexerError => {
                write!(
                    f,
                    "lexer error at {}..{}",
                    self.span.start, self.span.end
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'source> {
    tokens: Peekable<Lexer<'source>>,
    current_span: Span,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Self {
        Parser {
            tokens: Lexer::new(source).peekable(),
            current_span: 0..0,
        }
    }

    fn peek(&mut self) -> Option<&Result<Token, ()>> {
        self.tokens.peek().map(|(tok, _)| tok)
    }

    fn peek_token(&mut self) -> Option<Result<&Token, ()>> {
        self.tokens.peek().map(|(tok, _)| tok.as_ref().map_err(|_| ()))
    }

    fn advance(&mut self) -> Option<(Result<Token, ()>, Span)> {
        let result = self.tokens.next();
        if let Some((_, ref span)) = result {
            self.current_span = span.clone();
        }
        result
    }

    fn expect(&mut self, expected: Token) -> ParseResult<Span> {
        match self.advance() {
            Some((Ok(tok), span)) if tok == expected => Ok(span),
            Some((Ok(tok), span)) => Err(ParseError::unexpected_token(
                vec![&format!("{:?}", expected)],
                Some(tok),
                span,
            )),
            Some((Err(()), span)) => Err(ParseError::lexer_error(span)),
            None => Err(ParseError::unexpected_eof(
                vec![&format!("{:?}", expected)],
                self.current_span.clone(),
            )),
        }
    }

    fn expect_ident(&mut self) -> ParseResult<(String, Span)> {
        match self.advance() {
            Some((Ok(Token::Ident(name)), span)) => Ok((name, span)),
            Some((Ok(tok), span)) => Err(ParseError::unexpected_token(
                vec!["identifier"],
                Some(tok),
                span,
            )),
            Some((Err(()), span)) => Err(ParseError::lexer_error(span)),
            None => Err(ParseError::unexpected_eof(
                vec!["identifier"],
                self.current_span.clone(),
            )),
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut stmts = Vec::new();
        while self.peek().is_some() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(Program { stmts })
    }

    fn parse_stmt(&mut self) -> ParseResult<Spanned<Stmt>> {
        match self.peek_token() {
            Some(Ok(Token::Let)) => self.parse_let_stmt(),
            Some(Ok(Token::Fn)) => self.parse_fn_stmt(false),
            Some(Ok(Token::Tailrec)) => {
                let start_span = self.advance().unwrap().1; // consume 'tailrec'
                self.expect(Token::Fn)?;
                self.parse_fn_stmt_inner(true, start_span)
            }
            Some(Ok(Token::Enum)) => self.parse_enum_stmt(),
            Some(Ok(_)) => self.parse_expr_stmt(),
            Some(Err(())) => {
                let (_, span) = self.advance().unwrap();
                Err(ParseError::lexer_error(span))
            }
            None => Err(ParseError::unexpected_eof(
                vec!["statement"],
                self.current_span.clone(),
            )),
        }
    }

    fn parse_let_stmt(&mut self) -> ParseResult<Spanned<Stmt>> {
        let start_span = self.expect(Token::Let)?;
        let (name, _) = self.expect_ident()?;

        // Type annotation is optional: `let x: Int = 42;` or `let x = 42;`
        let ty = if matches!(self.peek_token(), Some(Ok(Token::Colon))) {
            self.advance(); // consume ':'
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::Assign)?;
        let value = self.parse_expr()?;
        let end_span = self.expect(Token::Semi)?;

        let span = start_span.start..end_span.end;
        Ok(Spanned::new(Stmt::Let { name, ty, value }, span))
    }

    fn parse_fn_stmt(&mut self, is_tailrec: bool) -> ParseResult<Spanned<Stmt>> {
        let start_span = self.expect(Token::Fn)?;
        self.parse_fn_stmt_inner(is_tailrec, start_span)
    }

    fn parse_fn_stmt_inner(&mut self, is_tailrec: bool, start_span: Span) -> ParseResult<Spanned<Stmt>> {
        let (name, _) = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;
        self.expect(Token::Arrow)?;
        let return_ty = self.parse_type()?;
        let body = self.parse_block_expr()?;

        let span = start_span.start..body.span.end;
        Ok(Spanned::new(
            Stmt::Fn {
                name,
                params,
                return_ty,
                body,
                is_tailrec,
            },
            span,
        ))
    }

    fn parse_params(&mut self) -> ParseResult<Vec<Spanned<Param>>> {
        let mut params = Vec::new();

        // Check for empty param list
        if matches!(self.peek_token(), Some(Ok(Token::RParen))) {
            return Ok(params);
        }

        loop {
            let (name, name_span) = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let ty = self.parse_type()?;
            let span = name_span.start..ty.span.end;
            params.push(Spanned::new(Param { name, ty }, span));

            match self.peek_token() {
                Some(Ok(Token::Comma)) => {
                    self.advance();
                }
                _ => break,
            }
        }

        Ok(params)
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Spanned<Stmt>> {
        let expr = self.parse_expr()?;
        let end_span = self.expect(Token::Semi)?;
        let span = expr.span.start..end_span.end;
        Ok(Spanned::new(Stmt::Expr(expr), span))
    }

    fn parse_type(&mut self) -> ParseResult<Spanned<Type>> {
        match self.advance() {
            Some((Ok(Token::IntType), span)) => Ok(Spanned::new(Type::Int, span)),
            Some((Ok(Token::FloatType), span)) => Ok(Spanned::new(Type::Float, span)),
            Some((Ok(Token::StringType), span)) => Ok(Spanned::new(Type::String, span)),
            Some((Ok(Token::BoolType), span)) => Ok(Spanned::new(Type::Bool, span)),
            // Named types (e.g., enum types like `Option` or `Color`)
            Some((Ok(Token::Ident(name)), span)) => Ok(Spanned::new(Type::Named(name), span)),
            Some((Ok(tok), span)) => Err(ParseError::unexpected_token(
                vec!["Int", "Float", "String", "Bool", "type name"],
                Some(tok),
                span,
            )),
            Some((Err(()), span)) => Err(ParseError::lexer_error(span)),
            None => Err(ParseError::unexpected_eof(
                vec!["Int", "Float", "String", "Bool", "type name"],
                self.current_span.clone(),
            )),
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        self.parse_expr_with_precedence(1)
    }

    fn parse_expr_with_precedence(&mut self, min_prec: u8) -> ParseResult<Spanned<Expr>> {
        let mut left = self.parse_primary()?;

        loop {
            let op = match self.peek_token() {
                Some(Ok(Token::Plus)) => BinOp::Add,
                Some(Ok(Token::Minus)) => BinOp::Sub,
                Some(Ok(Token::Star)) => BinOp::Mul,
                Some(Ok(Token::Slash)) => BinOp::Div,
                Some(Ok(Token::Percent)) => BinOp::Mod,
                Some(Ok(Token::Eq)) => BinOp::Eq,
                Some(Ok(Token::Ne)) => BinOp::Ne,
                Some(Ok(Token::Lt)) => BinOp::Lt,
                Some(Ok(Token::Gt)) => BinOp::Gt,
                Some(Ok(Token::Le)) => BinOp::Le,
                Some(Ok(Token::Ge)) => BinOp::Ge,
                _ => break,
            };

            let prec = op.precedence();
            if prec < min_prec {
                break;
            }

            // Consume the operator
            self.advance();

            // Left-associative: parse right side with min_prec + 1
            let right = self.parse_expr_with_precedence(prec + 1)?;

            let span = left.span.start..right.span.end;
            left = Spanned::new(
                Expr::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> ParseResult<Spanned<Expr>> {
        match self.peek_token() {
            Some(Ok(Token::Integer(_))) => self.parse_integer(),
            Some(Ok(Token::Float(_))) => self.parse_float(),
            Some(Ok(Token::String(_))) => self.parse_string(),
            Some(Ok(Token::True)) => self.parse_bool(true),
            Some(Ok(Token::False)) => self.parse_bool(false),
            Some(Ok(Token::Ident(_))) => self.parse_ident_or_call(),
            Some(Ok(Token::LParen)) => self.parse_grouped_expr(),
            Some(Ok(Token::LBrace)) => self.parse_block_expr(),
            Some(Ok(Token::If)) => self.parse_if_expr(),
            Some(Ok(Token::Match)) => self.parse_match_expr(),
            Some(Ok(_)) => {
                let (tok, span) = self.advance().unwrap();
                Err(ParseError::unexpected_token(
                    vec!["expression"],
                    tok.ok(),
                    span,
                ))
            }
            Some(Err(())) => {
                let (_, span) = self.advance().unwrap();
                Err(ParseError::lexer_error(span))
            }
            None => Err(ParseError::unexpected_eof(
                vec!["expression"],
                self.current_span.clone(),
            )),
        }
    }

    fn parse_integer(&mut self) -> ParseResult<Spanned<Expr>> {
        match self.advance() {
            Some((Ok(Token::Integer(n)), span)) => {
                Ok(Spanned::new(Expr::Literal(Literal::Integer(n)), span))
            }
            _ => unreachable!(),
        }
    }

    fn parse_float(&mut self) -> ParseResult<Spanned<Expr>> {
        match self.advance() {
            Some((Ok(Token::Float(n)), span)) => {
                Ok(Spanned::new(Expr::Literal(Literal::Float(n)), span))
            }
            _ => unreachable!(),
        }
    }

    fn parse_string(&mut self) -> ParseResult<Spanned<Expr>> {
        match self.advance() {
            Some((Ok(Token::String(s)), span)) => {
                Ok(Spanned::new(Expr::Literal(Literal::String(s)), span))
            }
            _ => unreachable!(),
        }
    }

    fn parse_bool(&mut self, value: bool) -> ParseResult<Spanned<Expr>> {
        let (_, span) = self.advance().unwrap();
        Ok(Spanned::new(Expr::Literal(Literal::Bool(value)), span))
    }

    fn parse_ident_or_call(&mut self) -> ParseResult<Spanned<Expr>> {
        let (name, start_span) = self.expect_ident()?;

        // Check if this is a function call
        if matches!(self.peek_token(), Some(Ok(Token::LParen))) {
            self.advance(); // consume '('
            let args = self.parse_args()?;
            let end_span = self.expect(Token::RParen)?;
            let span = start_span.start..end_span.end;
            Ok(Spanned::new(Expr::Call { callee: name, args }, span))
        } else {
            Ok(Spanned::new(Expr::Ident(name), start_span))
        }
    }

    fn parse_args(&mut self) -> ParseResult<Vec<Spanned<Expr>>> {
        let mut args = Vec::new();

        // Check for empty arg list
        if matches!(self.peek_token(), Some(Ok(Token::RParen))) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expr()?);

            match self.peek_token() {
                Some(Ok(Token::Comma)) => {
                    self.advance();
                }
                _ => break,
            }
        }

        Ok(args)
    }

    fn parse_grouped_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        self.expect(Token::LParen)?;
        let expr = self.parse_expr()?;
        self.expect(Token::RParen)?;
        Ok(expr)
    }

    fn parse_block_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start_span = self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        let mut final_expr = None;

        loop {
            // Check for closing brace
            if matches!(self.peek_token(), Some(Ok(Token::RBrace))) {
                break;
            }

            // Check for EOF
            if self.peek().is_none() {
                return Err(ParseError::unexpected_eof(
                    vec!["}"],
                    self.current_span.clone(),
                ));
            }

            // Try to parse a statement
            // Peek to see if this is a let, fn, tailrec, or enum statement
            match self.peek_token() {
                Some(Ok(Token::Let)) | Some(Ok(Token::Fn)) | Some(Ok(Token::Tailrec)) | Some(Ok(Token::Enum)) => {
                    stmts.push(self.parse_stmt()?);
                }
                _ => {
                    // Parse an expression
                    let expr = self.parse_expr()?;

                    // Check what follows
                    match self.peek_token() {
                        Some(Ok(Token::Semi)) => {
                            // Expression statement
                            let end_span = self.expect(Token::Semi)?;
                            let span = expr.span.start..end_span.end;
                            stmts.push(Spanned::new(Stmt::Expr(expr), span));
                        }
                        Some(Ok(Token::RBrace)) => {
                            // Final expression (no semicolon)
                            final_expr = Some(Box::new(expr));
                            break;
                        }
                        Some(Ok(_)) => {
                            let (tok, span) = self.advance().unwrap();
                            return Err(ParseError::unexpected_token(
                                vec![";", "}"],
                                tok.ok(),
                                span,
                            ));
                        }
                        Some(Err(())) => {
                            let (_, span) = self.advance().unwrap();
                            return Err(ParseError::lexer_error(span));
                        }
                        None => {
                            return Err(ParseError::unexpected_eof(
                                vec![";", "}"],
                                self.current_span.clone(),
                            ));
                        }
                    }
                }
            }
        }

        let end_span = self.expect(Token::RBrace)?;
        let span = start_span.start..end_span.end;
        Ok(Spanned::new(
            Expr::Block {
                stmts,
                expr: final_expr,
            },
            span,
        ))
    }

    fn parse_if_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start_span = self.expect(Token::If)?;
        let condition = self.parse_expr()?;
        let then_branch = self.parse_block_expr()?;
        self.expect(Token::Else)?;
        let else_branch = self.parse_block_expr()?;

        let span = start_span.start..else_branch.span.end;
        Ok(Spanned::new(
            Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            },
            span,
        ))
    }

    /// Parse an enum definition: `enum Color { Red, Green, Blue }`
    /// or with payloads: `enum Option { None, Some(Int) }`
    fn parse_enum_stmt(&mut self) -> ParseResult<Spanned<Stmt>> {
        let start_span = self.expect(Token::Enum)?;
        let (name, _) = self.expect_ident()?;
        self.expect(Token::LBrace)?;

        let mut variants = Vec::new();

        // Check for empty enum
        if !matches!(self.peek_token(), Some(Ok(Token::RBrace))) {
            loop {
                let (variant_name, variant_span_start) = self.expect_ident()?;

                // Check for payload type
                let payload = if matches!(self.peek_token(), Some(Ok(Token::LParen))) {
                    self.advance(); // consume '('
                    let payload_ty = self.parse_type()?;
                    self.expect(Token::RParen)?;
                    Some(payload_ty)
                } else {
                    None
                };

                let variant_span = variant_span_start.start..self.current_span.end;
                variants.push(Spanned::new(
                    EnumVariant {
                        name: variant_name,
                        payload,
                    },
                    variant_span,
                ));

                match self.peek_token() {
                    Some(Ok(Token::Comma)) => {
                        self.advance();
                        // Allow trailing comma
                        if matches!(self.peek_token(), Some(Ok(Token::RBrace))) {
                            break;
                        }
                    }
                    _ => break,
                }
            }
        }

        let end_span = self.expect(Token::RBrace)?;
        let span = start_span.start..end_span.end;
        Ok(Spanned::new(Stmt::Enum { name, variants }, span))
    }

    /// Parse a match expression: `match expr { pat => body, ... }`
    fn parse_match_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start_span = self.expect(Token::Match)?;
        let expr = self.parse_expr()?;
        self.expect(Token::LBrace)?;

        let mut arms = Vec::new();

        // Parse match arms
        while !matches!(self.peek_token(), Some(Ok(Token::RBrace))) {
            let pattern = self.parse_pattern()?;
            self.expect(Token::FatArrow)?;
            let body = self.parse_expr()?;

            let arm_span = pattern.span.start..body.span.end;
            arms.push(Spanned::new(MatchArm { pattern, body }, arm_span));

            // Optional comma between arms
            if matches!(self.peek_token(), Some(Ok(Token::Comma))) {
                self.advance();
            }

            // Check for end of match
            if matches!(self.peek_token(), Some(Ok(Token::RBrace))) {
                break;
            }
        }

        let end_span = self.expect(Token::RBrace)?;
        let span = start_span.start..end_span.end;
        Ok(Spanned::new(
            Expr::Match {
                expr: Box::new(expr),
                arms,
            },
            span,
        ))
    }

    /// Parse a pattern for match expressions
    fn parse_pattern(&mut self) -> ParseResult<Spanned<Pattern>> {
        match self.peek_token() {
            // Wildcard pattern: `_`
            Some(Ok(Token::Underscore)) => {
                let (_, span) = self.advance().unwrap();
                Ok(Spanned::new(Pattern::Wildcard, span))
            }
            // Literal patterns
            Some(Ok(Token::Integer(_))) => {
                let (tok, span) = self.advance().unwrap();
                if let Ok(Token::Integer(n)) = tok {
                    Ok(Spanned::new(Pattern::Literal(Literal::Integer(n)), span))
                } else {
                    unreachable!()
                }
            }
            Some(Ok(Token::Float(_))) => {
                let (tok, span) = self.advance().unwrap();
                if let Ok(Token::Float(f)) = tok {
                    Ok(Spanned::new(Pattern::Literal(Literal::Float(f)), span))
                } else {
                    unreachable!()
                }
            }
            Some(Ok(Token::String(_))) => {
                let (tok, span) = self.advance().unwrap();
                if let Ok(Token::String(s)) = tok {
                    Ok(Spanned::new(Pattern::Literal(Literal::String(s)), span))
                } else {
                    unreachable!()
                }
            }
            Some(Ok(Token::True)) => {
                let (_, span) = self.advance().unwrap();
                Ok(Spanned::new(Pattern::Literal(Literal::Bool(true)), span))
            }
            Some(Ok(Token::False)) => {
                let (_, span) = self.advance().unwrap();
                Ok(Spanned::new(Pattern::Literal(Literal::Bool(false)), span))
            }
            // Identifier pattern (variable binding) or enum variant pattern
            Some(Ok(Token::Ident(_))) => {
                let (tok, start_span) = self.advance().unwrap();
                let name = if let Ok(Token::Ident(n)) = tok {
                    n
                } else {
                    unreachable!()
                };

                // Check if this is an enum variant with payload: `Some(x)`
                if matches!(self.peek_token(), Some(Ok(Token::LParen))) {
                    self.advance(); // consume '('
                    let inner_pattern = self.parse_pattern()?;
                    let end_span = self.expect(Token::RParen)?;
                    let span = start_span.start..end_span.end;
                    Ok(Spanned::new(
                        Pattern::Variant {
                            name,
                            payload: Some(Box::new(inner_pattern)),
                        },
                        span,
                    ))
                } else {
                    // Could be either a variable binding or a unit variant
                    // We'll treat uppercase starting names as variants, lowercase as bindings
                    // This is a simple heuristic; a more robust solution would use context
                    if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                        Ok(Spanned::new(
                            Pattern::Variant {
                                name,
                                payload: None,
                            },
                            start_span,
                        ))
                    } else {
                        Ok(Spanned::new(Pattern::Ident(name), start_span))
                    }
                }
            }
            Some(Ok(_)) => {
                let (tok, span) = self.advance().unwrap();
                Err(ParseError::unexpected_token(
                    vec!["pattern"],
                    tok.ok(),
                    span,
                ))
            }
            Some(Err(())) => {
                let (_, span) = self.advance().unwrap();
                Err(ParseError::lexer_error(span))
            }
            None => Err(ParseError::unexpected_eof(
                vec!["pattern"],
                self.current_span.clone(),
            )),
        }
    }
}
