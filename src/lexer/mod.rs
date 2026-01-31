use logos::{Logos, Span};

#[cfg(test)]
mod tests;

fn parse_string(lex: &logos::Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    // Remove surrounding quotes
    Some(slice[1..slice.len() - 1].to_string())
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r\f]+")]
#[logos(skip r"//[^\n]*")]
pub enum Token {
    // Keywords
    #[token("let")]
    Let,

    #[token("fn")]
    Fn,

    #[token("tailrec")]
    Tailrec,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("enum")]
    Enum,

    #[token("match")]
    Match,

    #[token("yield")]
    Yield,

    #[token("gen")]
    Gen,

    #[token("async")]
    Async,

    #[token("await")]
    Await,

    // Type keywords
    #[token("Int")]
    IntType,

    #[token("Float")]
    FloatType,

    #[token("String")]
    StringType,

    #[token("Bool")]
    BoolType,

    // Literals
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse().ok(), priority = 2)]
    Integer(i64),

    #[regex(r"-?[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    Float(f64),

    #[regex(r#""[^"]*""#, parse_string)]
    String(String),

    // Identifier (must come after keywords to not shadow them)
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    // Comparison operators (multi-char must come before single-char)
    #[token("==")]
    Eq,

    #[token("!=")]
    Ne,

    #[token("<=")]
    Le,

    #[token(">=")]
    Ge,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    // Arrow (must come before Minus)
    #[token("->")]
    Arrow,

    // Fat arrow for match arms (must come before Assign)
    #[token("=>")]
    FatArrow,

    // Pipe for or-patterns
    #[token("|")]
    Pipe,

    // Underscore for wildcard patterns (higher priority than Ident)
    #[token("_", priority = 3)]
    Underscore,

    // Arithmetic operators
    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    // Assignment
    #[token("=")]
    Assign,

    // Delimiters
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token(";")]
    Semi,
}

pub struct Lexer<'source> {
    inner: logos::Lexer<'source, Token>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Lexer {
            inner: Token::lexer(source),
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = (Result<Token, ()>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner.next()?;
        let span = self.inner.span();
        Some((token, span))
    }
}
