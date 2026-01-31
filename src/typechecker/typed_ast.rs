use logos::Span;

use crate::ast::{BinOp, Literal};

use super::types::Type;

/// A typed expression with its inferred/checked type and source location.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Type,
    pub span: Span,
}

impl TypedExpr {
    pub fn new(kind: TypedExprKind, ty: Type, span: Span) -> Self {
        TypedExpr { kind, ty, span }
    }
}

/// The kind of typed expression.
#[derive(Debug, Clone, PartialEq)]
pub enum TypedExprKind {
    Literal(Literal),
    Ident(String),
    Binary {
        op: BinOp,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    Call {
        callee: String,
        args: Vec<TypedExpr>,
    },
    If {
        condition: Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Box<TypedExpr>,
    },
    Block {
        stmts: Vec<TypedStmt>,
        expr: Option<Box<TypedExpr>>,
    },
}

/// A typed statement with source location.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStmt {
    pub kind: TypedStmtKind,
    pub span: Span,
}

impl TypedStmt {
    pub fn new(kind: TypedStmtKind, span: Span) -> Self {
        TypedStmt { kind, span }
    }
}

/// The kind of typed statement.
#[derive(Debug, Clone, PartialEq)]
pub enum TypedStmtKind {
    Let {
        name: String,
        ty: Type,
        value: TypedExpr,
    },
    Fn {
        name: String,
        params: Vec<(String, Type)>,
        return_ty: Type,
        body: TypedExpr,
        is_tailrec: bool,
    },
    Expr(TypedExpr),
}

/// A fully typed program.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedProgram {
    pub stmts: Vec<TypedStmt>,
}

impl TypedProgram {
    pub fn new(stmts: Vec<TypedStmt>) -> Self {
        TypedProgram { stmts }
    }
}
