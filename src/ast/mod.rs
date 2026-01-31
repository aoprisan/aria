use logos::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    /// A named type (e.g., enum type like `Option` or `Color`)
    Named(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    // Precedence 3: multiplicative
    Mul,
    Div,
    Mod,
    // Precedence 2: additive
    Add,
    Sub,
    // Precedence 1: comparison
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl BinOp {
    pub fn precedence(self) -> u8 {
        match self {
            BinOp::Mul | BinOp::Div | BinOp::Mod => 3,
            BinOp::Add | BinOp::Sub => 2,
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

/// A pattern for pattern matching
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard pattern: `_`
    Wildcard,
    /// Literal pattern: `42`, `true`, `"hello"`
    Literal(Literal),
    /// Variable binding: `x`, `foo`
    Ident(String),
    /// Enum variant pattern: `None` or `Some(x)`
    Variant {
        name: String,
        payload: Option<Box<Spanned<Pattern>>>,
    },
}

/// A single arm of a match expression
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<Expr>,
}

/// A variant in an enum definition
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    /// Optional payload type for data-carrying variants
    pub payload: Option<Spanned<Type>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Binary {
        op: BinOp,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    Call {
        callee: String,
        args: Vec<Spanned<Expr>>,
    },
    If {
        condition: Box<Spanned<Expr>>,
        then_branch: Box<Spanned<Expr>>,
        else_branch: Box<Spanned<Expr>>,
    },
    Block {
        stmts: Vec<Spanned<Stmt>>,
        expr: Option<Box<Spanned<Expr>>>,
    },
    /// Match expression: `match expr { pat => body, ... }`
    Match {
        expr: Box<Spanned<Expr>>,
        arms: Vec<Spanned<MatchArm>>,
    },
    /// Enum variant constructor: `Some(42)` or `None`
    EnumVariant {
        variant: String,
        payload: Option<Box<Spanned<Expr>>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<Spanned<Type>>,
        value: Spanned<Expr>,
    },
    Fn {
        name: String,
        params: Vec<Spanned<Param>>,
        return_ty: Spanned<Type>,
        body: Spanned<Expr>,
        is_tailrec: bool,
    },
    Expr(Spanned<Expr>),
    /// Enum definition: `enum Color { Red, Green, Blue }`
    Enum {
        name: String,
        variants: Vec<Spanned<EnumVariant>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stmts: Vec<Spanned<Stmt>>,
}
