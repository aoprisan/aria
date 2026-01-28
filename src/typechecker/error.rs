use std::fmt;

use logos::Span;

use crate::ast::BinOp;

use super::types::Type;

/// Specific kind of type error that occurred.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeErrorKind {
    /// Expected one type but found another.
    TypeMismatch { expected: Type, found: Type },

    /// Referenced a variable that doesn't exist.
    UndefinedVariable { name: String },

    /// Called a function that doesn't exist.
    UndefinedFunction { name: String },

    /// Defined a variable/function that already exists in the same scope.
    DuplicateDefinition { name: String },

    /// Tried to call something that isn't a function.
    NotCallable { ty: Type },

    /// Called a function with wrong number of arguments.
    ArityMismatch { expected: usize, found: usize },

    /// Binary operator applied to incompatible types.
    BinaryOpTypeMismatch { op: BinOp, left: Type, right: Type },

    /// If branches have different types.
    IfBranchTypeMismatch { then_ty: Type, else_ty: Type },
}

/// A type error with source location information.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

impl TypeError {
    pub fn new(kind: TypeErrorKind, span: Span) -> Self {
        TypeError { kind, span }
    }

    pub fn type_mismatch(expected: Type, found: Type, span: Span) -> Self {
        TypeError::new(TypeErrorKind::TypeMismatch { expected, found }, span)
    }

    pub fn undefined_variable(name: String, span: Span) -> Self {
        TypeError::new(TypeErrorKind::UndefinedVariable { name }, span)
    }

    pub fn undefined_function(name: String, span: Span) -> Self {
        TypeError::new(TypeErrorKind::UndefinedFunction { name }, span)
    }

    pub fn duplicate_definition(name: String, span: Span) -> Self {
        TypeError::new(TypeErrorKind::DuplicateDefinition { name }, span)
    }

    pub fn not_callable(ty: Type, span: Span) -> Self {
        TypeError::new(TypeErrorKind::NotCallable { ty }, span)
    }

    pub fn arity_mismatch(expected: usize, found: usize, span: Span) -> Self {
        TypeError::new(TypeErrorKind::ArityMismatch { expected, found }, span)
    }

    pub fn binary_op_mismatch(op: BinOp, left: Type, right: Type, span: Span) -> Self {
        TypeError::new(TypeErrorKind::BinaryOpTypeMismatch { op, left, right }, span)
    }

    pub fn if_branch_mismatch(then_ty: Type, else_ty: Type, span: Span) -> Self {
        TypeError::new(TypeErrorKind::IfBranchTypeMismatch { then_ty, else_ty }, span)
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type error at {}..{}: ", self.span.start, self.span.end)?;
        match &self.kind {
            TypeErrorKind::TypeMismatch { expected, found } => {
                write!(f, "expected {}, found {}", expected, found)
            }
            TypeErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable '{}'", name)
            }
            TypeErrorKind::UndefinedFunction { name } => {
                write!(f, "undefined function '{}'", name)
            }
            TypeErrorKind::DuplicateDefinition { name } => {
                write!(f, "duplicate definition of '{}'", name)
            }
            TypeErrorKind::NotCallable { ty } => {
                write!(f, "type {} is not callable", ty)
            }
            TypeErrorKind::ArityMismatch { expected, found } => {
                write!(
                    f,
                    "function expects {} argument(s), but {} were provided",
                    expected, found
                )
            }
            TypeErrorKind::BinaryOpTypeMismatch { op, left, right } => {
                write!(
                    f,
                    "cannot apply {:?} to {} and {}",
                    op, left, right
                )
            }
            TypeErrorKind::IfBranchTypeMismatch { then_ty, else_ty } => {
                write!(
                    f,
                    "if branches have incompatible types: then branch is {}, else branch is {}",
                    then_ty, else_ty
                )
            }
        }
    }
}

impl std::error::Error for TypeError {}
