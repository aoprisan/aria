use std::fmt;

use crate::ast;

/// Represents a single variant in an enum type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantDef {
    pub name: String,
    pub payload: Option<Box<Type>>,
}

/// Internal type representation for the type checker.
/// Extends the AST Type with Unit and Function types.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unit,
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
    },
    /// Enum type with its name and variant definitions
    Enum {
        name: String,
        variants: Vec<EnumVariantDef>,
    },
}

impl Type {
    /// Convert from AST type to type checker type.
    /// Note: Named types cannot be fully resolved without context;
    /// they are handled specially in the type checker.
    pub fn from_ast(ast_type: &ast::Type) -> Self {
        match ast_type {
            ast::Type::Int => Type::Int,
            ast::Type::Float => Type::Float,
            ast::Type::String => Type::String,
            ast::Type::Bool => Type::Bool,
            ast::Type::Named(name) => {
                // Named types need to be resolved by the type checker
                // We create a placeholder enum type; the type checker will resolve it
                Type::Enum {
                    name: name.clone(),
                    variants: Vec::new(), // Will be filled in by type checker
                }
            }
        }
    }

    /// Get a human-readable name for error messages.
    pub fn display_name(&self) -> String {
        match self {
            Type::Int => "Int".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::Function { params, ret } => {
                let params_str: Vec<String> =
                    params.iter().map(|p| p.display_name()).collect();
                format!("fn({}) -> {}", params_str.join(", "), ret.display_name())
            }
            Type::Enum { name, .. } => name.clone(),
        }
    }

    /// Check if this type is the same enum type as another (by name)
    pub fn is_same_enum(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,
            _ => false,
        }
    }

    /// Check if this type is numeric (Int or Float).
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_name())
    }
}
