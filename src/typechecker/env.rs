use std::collections::HashMap;

use crate::ast::{Spanned, Stmt};

use super::types::{EnumVariantDef, Type};

/// Information about a generic function definition.
#[derive(Debug, Clone)]
pub struct GenericFunctionDef {
    pub type_params: Vec<String>,
    pub ast: Spanned<Stmt>,
}

/// Environment for tracking variable bindings across scopes.
#[derive(Debug, Clone)]
pub struct Env {
    /// Stack of scopes, from outermost to innermost.
    /// Each scope maps variable names to their types.
    scopes: Vec<HashMap<String, Type>>,
    /// Registered enum types (name -> full enum type with variants)
    enums: HashMap<String, Type>,
    /// Generic function definitions (for later monomorphization)
    generic_functions: HashMap<String, GenericFunctionDef>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            scopes: vec![HashMap::new()],
            enums: HashMap::new(),
            generic_functions: HashMap::new(),
        }
    }

    /// Enter a new scope (e.g., entering a block).
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope (e.g., leaving a block).
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a binding in the current scope.
    /// Returns false if the name already exists in the current scope (duplicate).
    pub fn define(&mut self, name: String, ty: Type) -> bool {
        let current_scope = self.scopes.last_mut().expect("at least one scope exists");
        if current_scope.contains_key(&name) {
            false
        } else {
            current_scope.insert(name, ty);
            true
        }
    }

    /// Look up a binding by name, searching from innermost to outermost scope.
    /// Returns None if not found.
    pub fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    /// Check if a name exists in the current scope (not outer scopes).
    pub fn exists_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .map(|s| s.contains_key(name))
            .unwrap_or(false)
    }

    /// Register an enum type.
    /// Returns false if an enum with this name already exists.
    pub fn define_enum(&mut self, name: String, enum_type: Type) -> bool {
        if self.enums.contains_key(&name) {
            false
        } else {
            self.enums.insert(name, enum_type);
            true
        }
    }

    /// Look up an enum type by name.
    pub fn lookup_enum(&self, name: &str) -> Option<&Type> {
        self.enums.get(name)
    }

    /// Look up an enum variant by variant name.
    /// Returns (enum_name, variant_index, variant_def) if found.
    pub fn lookup_variant(&self, variant_name: &str) -> Option<(&str, usize, &EnumVariantDef)> {
        for (enum_name, ty) in &self.enums {
            if let Type::Enum { variants, .. } = ty {
                for (idx, variant) in variants.iter().enumerate() {
                    if variant.name == variant_name {
                        return Some((enum_name, idx, variant));
                    }
                }
            }
        }
        None
    }

    /// Resolve a named type to its full enum type (if it's an enum).
    pub fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Enum { name, variants } if variants.is_empty() => {
                // This is a placeholder type from Type::from_ast
                // Try to resolve it to the full enum type
                if let Some(full_type) = self.lookup_enum(name) {
                    full_type.clone()
                } else {
                    ty.clone()
                }
            }
            _ => ty.clone(),
        }
    }

    /// Register a generic function definition.
    /// Returns false if a function with this name already exists.
    pub fn define_generic_function(
        &mut self,
        name: String,
        type_params: Vec<String>,
        ast: Spanned<Stmt>,
    ) -> bool {
        if self.generic_functions.contains_key(&name) || self.lookup(&name).is_some() {
            false
        } else {
            self.generic_functions.insert(
                name,
                GenericFunctionDef { type_params, ast },
            );
            true
        }
    }

    /// Look up a generic function definition by name.
    pub fn lookup_generic_function(&self, name: &str) -> Option<&GenericFunctionDef> {
        self.generic_functions.get(name)
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
