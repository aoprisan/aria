use std::fmt;

use crate::ast;

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
}

impl Type {
    /// Convert from AST type to type checker type.
    pub fn from_ast(ast_type: &ast::Type) -> Self {
        match ast_type {
            ast::Type::Int => Type::Int,
            ast::Type::Float => Type::Float,
            ast::Type::String => Type::String,
            ast::Type::Bool => Type::Bool,
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
