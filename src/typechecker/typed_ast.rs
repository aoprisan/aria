use logos::Span;

use crate::ast::{BinOp, Literal};

use super::types::{EnumVariantDef, Type};

/// A typed pattern for pattern matching
#[derive(Debug, Clone, PartialEq)]
pub struct TypedPattern {
    pub kind: TypedPatternKind,
    pub ty: Type,
    pub span: Span,
}

impl TypedPattern {
    pub fn new(kind: TypedPatternKind, ty: Type, span: Span) -> Self {
        TypedPattern { kind, ty, span }
    }
}

/// The kind of typed pattern
#[derive(Debug, Clone, PartialEq)]
pub enum TypedPatternKind {
    /// Wildcard pattern: `_`
    Wildcard,
    /// Literal pattern: `42`, `true`
    Literal(Literal),
    /// Variable binding: introduces a new variable
    Ident(String),
    /// Enum variant pattern: `None` or `Some(x)`
    Variant {
        enum_name: String,
        variant_name: String,
        variant_index: usize,
        payload: Option<Box<TypedPattern>>,
    },
}

/// A typed match arm
#[derive(Debug, Clone, PartialEq)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub body: TypedExpr,
}

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
        /// Whether this call is in tail position (for tail-call optimization)
        is_tail_call: bool,
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
    /// Match expression
    Match {
        expr: Box<TypedExpr>,
        arms: Vec<TypedMatchArm>,
    },
    /// Enum variant constructor
    EnumVariant {
        enum_name: String,
        variant_name: String,
        variant_index: usize,
        payload: Option<Box<TypedExpr>>,
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
    /// Enum definition
    Enum {
        name: String,
        variants: Vec<EnumVariantDef>,
    },
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
