use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum CodeGenError {
    UnsupportedType(String),
    UnsupportedFeature(String),
    UndefinedVariable(String),
    UndefinedFunction(String),
}

impl fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodeGenError::UnsupportedType(ty) => {
                write!(f, "unsupported type for WASM codegen: {}", ty)
            }
            CodeGenError::UnsupportedFeature(feature) => {
                write!(f, "unsupported feature for WASM codegen: {}", feature)
            }
            CodeGenError::UndefinedVariable(name) => {
                write!(f, "undefined variable: {}", name)
            }
            CodeGenError::UndefinedFunction(name) => {
                write!(f, "undefined function: {}", name)
            }
        }
    }
}

impl std::error::Error for CodeGenError {}
