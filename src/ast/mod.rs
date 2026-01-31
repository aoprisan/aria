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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stmts: Vec<Spanned<Stmt>>,
}
