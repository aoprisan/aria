use std::collections::HashMap;

use super::types::Type;

/// Environment for tracking variable bindings across scopes.
#[derive(Debug, Clone)]
pub struct Env {
    /// Stack of scopes, from outermost to innermost.
    /// Each scope maps variable names to their types.
    scopes: Vec<HashMap<String, Type>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            scopes: vec![HashMap::new()],
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
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
