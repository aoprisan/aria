pub mod env;
pub mod error;
pub mod typed_ast;
pub mod types;

#[cfg(test)]
mod tests;

use crate::ast::{BinOp, Expr, Literal, Program, Spanned, Stmt};

use self::env::Env;
use self::error::TypeError;
use self::typed_ast::{TypedExpr, TypedExprKind, TypedProgram, TypedStmt, TypedStmtKind};
use self::types::Type;

/// Bidirectional type checker for Aria programs.
pub struct TypeChecker {
    env: Env,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: Env::new(),
            errors: Vec::new(),
        }
    }

    /// Type check a program, returning either a typed AST or accumulated errors.
    pub fn check(&mut self, program: &Program) -> Result<TypedProgram, Vec<TypeError>> {
        // First pass: register all top-level function signatures
        for stmt in &program.stmts {
            if let Stmt::Fn {
                name,
                params,
                return_ty,
                ..
            } = &stmt.node
            {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| Type::from_ast(&p.node.ty.node))
                    .collect();
                let ret = Type::from_ast(&return_ty.node);
                let fn_type = Type::Function {
                    params: param_types,
                    ret: Box::new(ret),
                };
                if !self.env.define(name.clone(), fn_type) {
                    self.errors.push(TypeError::duplicate_definition(
                        name.clone(),
                        stmt.span.clone(),
                    ));
                }
            }
        }

        // Second pass: type check all statements
        let mut typed_stmts = Vec::new();
        for stmt in &program.stmts {
            match self.check_stmt(stmt) {
                Ok(typed_stmt) => typed_stmts.push(typed_stmt),
                Err(e) => self.errors.push(e),
            }
        }

        if self.errors.is_empty() {
            Ok(TypedProgram::new(typed_stmts))
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<TypedStmt, TypeError> {
        match &stmt.node {
            Stmt::Let { name, ty, value } => self.check_let_stmt(name, ty, value, stmt.span.clone()),
            Stmt::Fn {
                name,
                params,
                return_ty,
                body,
            } => self.check_fn_stmt(name, params, return_ty, body, stmt.span.clone()),
            Stmt::Expr(expr) => {
                let typed_expr = self.check_expr(expr)?;
                Ok(TypedStmt::new(TypedStmtKind::Expr(typed_expr), stmt.span.clone()))
            }
        }
    }

    fn check_let_stmt(
        &mut self,
        name: &str,
        ty: &Option<Spanned<crate::ast::Type>>,
        value: &Spanned<Expr>,
        span: logos::Span,
    ) -> Result<TypedStmt, TypeError> {
        let (typed_value, var_type) = match ty {
            // Explicit type annotation: check expression against it
            Some(ty_spanned) => {
                let expected = Type::from_ast(&ty_spanned.node);
                let typed_value = self.check_expr_expecting(value, &expected)?;
                (typed_value, expected)
            }
            // No annotation: infer from value
            None => {
                let typed_value = self.check_expr(value)?;
                let inferred = typed_value.ty.clone();
                (typed_value, inferred)
            }
        };

        // Check for duplicate in current scope
        if !self.env.define(name.to_string(), var_type.clone()) {
            return Err(TypeError::duplicate_definition(name.to_string(), span.clone()));
        }

        Ok(TypedStmt::new(
            TypedStmtKind::Let {
                name: name.to_string(),
                ty: var_type,
                value: typed_value,
            },
            span,
        ))
    }

    fn check_fn_stmt(
        &mut self,
        name: &str,
        params: &[Spanned<crate::ast::Param>],
        return_ty: &Spanned<crate::ast::Type>,
        body: &Spanned<Expr>,
        span: logos::Span,
    ) -> Result<TypedStmt, TypeError> {
        let ret_type = Type::from_ast(&return_ty.node);

        // Enter new scope for function body
        self.env.push_scope();

        // Bind parameters
        let mut typed_params = Vec::new();
        for param in params {
            let param_type = Type::from_ast(&param.node.ty.node);
            let param_name = param.node.name.clone();
            self.env.define(param_name.clone(), param_type.clone());
            typed_params.push((param_name, param_type));
        }

        // Check body against return type
        let typed_body = self.check_expr_expecting(body, &ret_type)?;

        // Exit scope
        self.env.pop_scope();

        Ok(TypedStmt::new(
            TypedStmtKind::Fn {
                name: name.to_string(),
                params: typed_params,
                return_ty: ret_type,
                body: typed_body,
            },
            span,
        ))
    }

    /// Synthesize a type for an expression (inference mode).
    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Result<TypedExpr, TypeError> {
        match &expr.node {
            Expr::Literal(lit) => {
                let ty = self.type_of_literal(lit);
                Ok(TypedExpr::new(
                    TypedExprKind::Literal(lit.clone()),
                    ty,
                    expr.span.clone(),
                ))
            }
            Expr::Ident(name) => {
                match self.env.lookup(name) {
                    Some(ty) => Ok(TypedExpr::new(
                        TypedExprKind::Ident(name.clone()),
                        ty.clone(),
                        expr.span.clone(),
                    )),
                    None => Err(TypeError::undefined_variable(name.clone(), expr.span.clone())),
                }
            }
            Expr::Binary { op, left, right } => self.check_binary(*op, left, right, expr.span.clone()),
            Expr::Call { callee, args } => self.check_call(callee, args, expr.span.clone()),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => self.check_if(condition, then_branch, else_branch, expr.span.clone()),
            Expr::Block { stmts, expr: final_expr } => {
                self.check_block(stmts, final_expr, expr.span.clone())
            }
        }
    }

    /// Check an expression against an expected type.
    fn check_expr_expecting(
        &mut self,
        expr: &Spanned<Expr>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeError> {
        let typed = self.check_expr(expr)?;
        if typed.ty != *expected {
            Err(TypeError::type_mismatch(
                expected.clone(),
                typed.ty.clone(),
                expr.span.clone(),
            ))
        } else {
            Ok(typed)
        }
    }

    fn type_of_literal(&self, lit: &Literal) -> Type {
        match lit {
            Literal::Integer(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::String(_) => Type::String,
            Literal::Bool(_) => Type::Bool,
        }
    }

    fn check_binary(
        &mut self,
        op: BinOp,
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        let typed_left = self.check_expr(left)?;
        let typed_right = self.check_expr(right)?;

        let result_type = self.type_of_binary_op(
            op,
            &typed_left.ty,
            &typed_right.ty,
            span.clone(),
        )?;

        Ok(TypedExpr::new(
            TypedExprKind::Binary {
                op,
                left: Box::new(typed_left),
                right: Box::new(typed_right),
            },
            result_type,
            span,
        ))
    }

    fn type_of_binary_op(
        &self,
        op: BinOp,
        left: &Type,
        right: &Type,
        span: logos::Span,
    ) -> Result<Type, TypeError> {
        match op {
            // Arithmetic: Int×Int→Int, Float×Float→Float, String+String→String
            BinOp::Add => match (left, right) {
                (Type::Int, Type::Int) => Ok(Type::Int),
                (Type::Float, Type::Float) => Ok(Type::Float),
                (Type::String, Type::String) => Ok(Type::String),
                _ => Err(TypeError::binary_op_mismatch(op, left.clone(), right.clone(), span)),
            },
            BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => match (left, right) {
                (Type::Int, Type::Int) => Ok(Type::Int),
                (Type::Float, Type::Float) => Ok(Type::Float),
                _ => Err(TypeError::binary_op_mismatch(op, left.clone(), right.clone(), span)),
            },
            // Comparison: Int×Int→Bool, Float×Float→Bool
            BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => match (left, right) {
                (Type::Int, Type::Int) => Ok(Type::Bool),
                (Type::Float, Type::Float) => Ok(Type::Bool),
                _ => Err(TypeError::binary_op_mismatch(op, left.clone(), right.clone(), span)),
            },
            // Equality: T×T→Bool (same types only)
            BinOp::Eq | BinOp::Ne => {
                if left == right {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::binary_op_mismatch(op, left.clone(), right.clone(), span))
                }
            }
        }
    }

    fn check_call(
        &mut self,
        callee: &str,
        args: &[Spanned<Expr>],
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        let fn_type = match self.env.lookup(callee) {
            Some(ty) => ty.clone(),
            None => return Err(TypeError::undefined_function(callee.to_string(), span)),
        };

        match fn_type {
            Type::Function { params, ret } => {
                // Check arity
                if args.len() != params.len() {
                    return Err(TypeError::arity_mismatch(params.len(), args.len(), span));
                }

                // Check each argument against its expected type
                let mut typed_args = Vec::new();
                for (arg, expected_ty) in args.iter().zip(params.iter()) {
                    let typed_arg = self.check_expr_expecting(arg, expected_ty)?;
                    typed_args.push(typed_arg);
                }

                Ok(TypedExpr::new(
                    TypedExprKind::Call {
                        callee: callee.to_string(),
                        args: typed_args,
                    },
                    *ret,
                    span,
                ))
            }
            other => Err(TypeError::not_callable(other, span)),
        }
    }

    fn check_if(
        &mut self,
        condition: &Spanned<Expr>,
        then_branch: &Spanned<Expr>,
        else_branch: &Spanned<Expr>,
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        // Condition must be Bool
        let typed_cond = self.check_expr_expecting(condition, &Type::Bool)?;

        // Both branches must have the same type
        let typed_then = self.check_expr(then_branch)?;
        let typed_else = self.check_expr(else_branch)?;

        if typed_then.ty != typed_else.ty {
            return Err(TypeError::if_branch_mismatch(
                typed_then.ty.clone(),
                typed_else.ty.clone(),
                span,
            ));
        }

        let result_ty = typed_then.ty.clone();

        Ok(TypedExpr::new(
            TypedExprKind::If {
                condition: Box::new(typed_cond),
                then_branch: Box::new(typed_then),
                else_branch: Box::new(typed_else),
            },
            result_ty,
            span,
        ))
    }

    fn check_block(
        &mut self,
        stmts: &[Spanned<Stmt>],
        final_expr: &Option<Box<Spanned<Expr>>>,
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        self.env.push_scope();

        let mut typed_stmts = Vec::new();
        for stmt in stmts {
            let typed_stmt = self.check_stmt(stmt)?;
            typed_stmts.push(typed_stmt);
        }

        let (typed_final_expr, block_type) = match final_expr {
            Some(expr) => {
                let typed = self.check_expr(expr)?;
                let ty = typed.ty.clone();
                (Some(Box::new(typed)), ty)
            }
            None => (None, Type::Unit),
        };

        self.env.pop_scope();

        Ok(TypedExpr::new(
            TypedExprKind::Block {
                stmts: typed_stmts,
                expr: typed_final_expr,
            },
            block_type,
            span,
        ))
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
