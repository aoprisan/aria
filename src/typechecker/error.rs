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

    /// Recursive call in tailrec function is not in tail position.
    NotTailRecursive { func_name: String },

    /// Referenced an undefined enum type.
    UndefinedEnum { name: String },

    /// Referenced an undefined enum variant.
    UndefinedVariant { enum_name: String, variant_name: String },

    /// Match arms have different types.
    MatchArmTypeMismatch { first_ty: Type, arm_ty: Type },

    /// Match expression is not exhaustive.
    NonExhaustiveMatch { missing_variants: Vec<String> },

    /// Pattern doesn't match the expected type.
    PatternTypeMismatch { expected: Type, found: Type },

    /// Variant expects a payload but none was provided.
    MissingVariantPayload { variant_name: String },

    /// Variant doesn't expect a payload but one was provided.
    UnexpectedVariantPayload { variant_name: String },

    /// Generic function called without explicit type arguments and inference failed.
    TypeInferenceFailed { func_name: String, type_params: Vec<String> },

    /// Wrong number of type arguments for a generic function.
    WrongNumberOfTypeArgs { func_name: String, expected: usize, found: usize },

    /// Yield expression used outside of a generator function.
    YieldOutsideGenerator,

    /// Yield expression has wrong type for this generator.
    YieldTypeMismatch { expected: Type, found: Type },
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

    pub fn not_tail_recursive(func_name: String, span: Span) -> Self {
        TypeError::new(TypeErrorKind::NotTailRecursive { func_name }, span)
    }

    pub fn undefined_enum(name: String, span: Span) -> Self {
        TypeError::new(TypeErrorKind::UndefinedEnum { name }, span)
    }

    pub fn undefined_variant(enum_name: String, variant_name: String, span: Span) -> Self {
        TypeError::new(
            TypeErrorKind::UndefinedVariant {
                enum_name,
                variant_name,
            },
            span,
        )
    }

    pub fn match_arm_mismatch(first_ty: Type, arm_ty: Type, span: Span) -> Self {
        TypeError::new(TypeErrorKind::MatchArmTypeMismatch { first_ty, arm_ty }, span)
    }

    pub fn non_exhaustive_match(missing_variants: Vec<String>, span: Span) -> Self {
        TypeError::new(TypeErrorKind::NonExhaustiveMatch { missing_variants }, span)
    }

    pub fn pattern_type_mismatch(expected: Type, found: Type, span: Span) -> Self {
        TypeError::new(TypeErrorKind::PatternTypeMismatch { expected, found }, span)
    }

    pub fn missing_variant_payload(variant_name: String, span: Span) -> Self {
        TypeError::new(TypeErrorKind::MissingVariantPayload { variant_name }, span)
    }

    pub fn unexpected_variant_payload(variant_name: String, span: Span) -> Self {
        TypeError::new(TypeErrorKind::UnexpectedVariantPayload { variant_name }, span)
    }

    pub fn type_inference_failed(func_name: String, type_params: Vec<String>, span: Span) -> Self {
        TypeError::new(TypeErrorKind::TypeInferenceFailed { func_name, type_params }, span)
    }

    pub fn wrong_number_of_type_args(func_name: String, expected: usize, found: usize, span: Span) -> Self {
        TypeError::new(TypeErrorKind::WrongNumberOfTypeArgs { func_name, expected, found }, span)
    }

    pub fn yield_outside_generator(span: Span) -> Self {
        TypeError::new(TypeErrorKind::YieldOutsideGenerator, span)
    }

    pub fn yield_type_mismatch(expected: Type, found: Type, span: Span) -> Self {
        TypeError::new(TypeErrorKind::YieldTypeMismatch { expected, found }, span)
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
            TypeErrorKind::NotTailRecursive { func_name } => {
                write!(
                    f,
                    "recursive call to '{}' is not in tail position",
                    func_name
                )
            }
            TypeErrorKind::UndefinedEnum { name } => {
                write!(f, "undefined enum type '{}'", name)
            }
            TypeErrorKind::UndefinedVariant {
                enum_name,
                variant_name,
            } => {
                write!(
                    f,
                    "enum '{}' has no variant named '{}'",
                    enum_name, variant_name
                )
            }
            TypeErrorKind::MatchArmTypeMismatch { first_ty, arm_ty } => {
                write!(
                    f,
                    "match arm has type {}, but first arm has type {}",
                    arm_ty, first_ty
                )
            }
            TypeErrorKind::NonExhaustiveMatch { missing_variants } => {
                write!(
                    f,
                    "non-exhaustive match: missing variants {:?}",
                    missing_variants
                )
            }
            TypeErrorKind::PatternTypeMismatch { expected, found } => {
                write!(
                    f,
                    "pattern has type {}, but expected {}",
                    found, expected
                )
            }
            TypeErrorKind::MissingVariantPayload { variant_name } => {
                write!(
                    f,
                    "variant '{}' expects a payload, but none was provided",
                    variant_name
                )
            }
            TypeErrorKind::UnexpectedVariantPayload { variant_name } => {
                write!(
                    f,
                    "variant '{}' does not expect a payload",
                    variant_name
                )
            }
            TypeErrorKind::TypeInferenceFailed { func_name, type_params } => {
                write!(
                    f,
                    "cannot infer type arguments for generic function '{}', please specify: {}<{}>(...)",
                    func_name, func_name, type_params.join(", ")
                )
            }
            TypeErrorKind::WrongNumberOfTypeArgs { func_name, expected, found } => {
                write!(
                    f,
                    "generic function '{}' expects {} type argument(s), but {} were provided",
                    func_name, expected, found
                )
            }
            TypeErrorKind::YieldOutsideGenerator => {
                write!(f, "yield expression can only be used inside a generator function (use 'gen fn')")
            }
            TypeErrorKind::YieldTypeMismatch { expected, found } => {
                write!(
                    f,
                    "yield expression has type {}, but generator expects {}",
                    found, expected
                )
            }
        }
    }
}

impl std::error::Error for TypeError {}
