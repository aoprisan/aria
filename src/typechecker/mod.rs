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
    /// Current function context for tailrec checking
    current_function: Option<CurrentFunction>,
}

/// Context for the function currently being type-checked.
#[derive(Clone)]
struct CurrentFunction {
    name: String,
    is_tailrec: bool,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: Env::new(),
            errors: Vec::new(),
            current_function: None,
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
                is_tailrec,
            } => self.check_fn_stmt(name, params, return_ty, body, *is_tailrec, stmt.span.clone()),
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
        is_tailrec: bool,
        span: logos::Span,
    ) -> Result<TypedStmt, TypeError> {
        let ret_type = Type::from_ast(&return_ty.node);

        // Enter new scope for function body
        self.env.push_scope();

        // Set current function context
        let old_context = self.current_function.take();
        self.current_function = Some(CurrentFunction {
            name: name.to_string(),
            is_tailrec,
        });

        // Bind parameters
        let mut typed_params = Vec::new();
        for param in params {
            let param_type = Type::from_ast(&param.node.ty.node);
            let param_name = param.node.name.clone();
            self.env.define(param_name.clone(), param_type.clone());
            typed_params.push((param_name, param_type));
        }

        // Check body against return type
        // For tailrec functions, we check tail position
        let typed_body = if is_tailrec {
            self.check_expr_expecting_tailrec(body, &ret_type, true)?
        } else {
            self.check_expr_expecting(body, &ret_type)?
        };

        // Restore old context
        self.current_function = old_context;

        // Exit scope
        self.env.pop_scope();

        Ok(TypedStmt::new(
            TypedStmtKind::Fn {
                name: name.to_string(),
                params: typed_params,
                return_ty: ret_type,
                body: typed_body,
                is_tailrec,
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

    /// Check an expression in a tailrec context.
    /// `in_tail_position` indicates if this expression is in tail position.
    fn check_expr_expecting_tailrec(
        &mut self,
        expr: &Spanned<Expr>,
        expected: &Type,
        in_tail_position: bool,
    ) -> Result<TypedExpr, TypeError> {
        let typed = self.check_expr_tailrec(expr, in_tail_position)?;
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

    /// Check an expression, verifying tail recursion constraints.
    fn check_expr_tailrec(
        &mut self,
        expr: &Spanned<Expr>,
        in_tail_position: bool,
    ) -> Result<TypedExpr, TypeError> {
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
            Expr::Binary { op, left, right } => {
                // Binary operations: neither operand is in tail position
                self.check_binary_tailrec(*op, left, right, expr.span.clone())
            }
            Expr::Call { callee, args } => {
                self.check_call_tailrec(callee, args, expr.span.clone(), in_tail_position)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => self.check_if_tailrec(condition, then_branch, else_branch, expr.span.clone(), in_tail_position),
            Expr::Block { stmts, expr: final_expr } => {
                self.check_block_tailrec(stmts, final_expr, expr.span.clone(), in_tail_position)
            }
        }
    }

    /// Check a binary expression in tailrec context (operands are not in tail position).
    fn check_binary_tailrec(
        &mut self,
        op: BinOp,
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        // Neither operand of a binary op is in tail position
        let typed_left = self.check_expr_tailrec(left, false)?;
        let typed_right = self.check_expr_tailrec(right, false)?;

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

    /// Check a function call in tailrec context.
    fn check_call_tailrec(
        &mut self,
        callee: &str,
        args: &[Spanned<Expr>],
        span: logos::Span,
        in_tail_position: bool,
    ) -> Result<TypedExpr, TypeError> {
        let fn_type = match self.env.lookup(callee) {
            Some(ty) => ty.clone(),
            None => return Err(TypeError::undefined_function(callee.to_string(), span)),
        };

        match fn_type {
            Type::Function { params, ret } => {
                // Check if this is a recursive call to the current function
                let is_recursive_call = self
                    .current_function
                    .as_ref()
                    .map(|f| f.name == callee && f.is_tailrec)
                    .unwrap_or(false);

                // If it's a recursive call and not in tail position, error
                if is_recursive_call && !in_tail_position {
                    return Err(TypeError::not_tail_recursive(callee.to_string(), span));
                }

                // Check arity
                if args.len() != params.len() {
                    return Err(TypeError::arity_mismatch(params.len(), args.len(), span));
                }

                // Check each argument (arguments are NOT in tail position)
                let mut typed_args = Vec::new();
                for (arg, expected_ty) in args.iter().zip(params.iter()) {
                    let typed_arg = self.check_expr_expecting_tailrec(arg, expected_ty, false)?;
                    typed_args.push(typed_arg);
                }

                Ok(TypedExpr::new(
                    TypedExprKind::Call {
                        callee: callee.to_string(),
                        args: typed_args,
                        is_tail_call: in_tail_position,
                    },
                    *ret,
                    span,
                ))
            }
            other => Err(TypeError::not_callable(other, span)),
        }
    }

    /// Check an if expression in tailrec context.
    fn check_if_tailrec(
        &mut self,
        condition: &Spanned<Expr>,
        then_branch: &Spanned<Expr>,
        else_branch: &Spanned<Expr>,
        span: logos::Span,
        in_tail_position: bool,
    ) -> Result<TypedExpr, TypeError> {
        // Condition is NOT in tail position
        let typed_cond = self.check_expr_expecting_tailrec(condition, &Type::Bool, false)?;

        // Both branches inherit the tail position status
        let typed_then = self.check_expr_tailrec(then_branch, in_tail_position)?;
        let typed_else = self.check_expr_tailrec(else_branch, in_tail_position)?;

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

    /// Check a block in tailrec context.
    fn check_block_tailrec(
        &mut self,
        stmts: &[Spanned<Stmt>],
        final_expr: &Option<Box<Spanned<Expr>>>,
        span: logos::Span,
        in_tail_position: bool,
    ) -> Result<TypedExpr, TypeError> {
        self.env.push_scope();

        let mut typed_stmts = Vec::new();
        for stmt in stmts {
            let typed_stmt = self.check_stmt(stmt)?;
            typed_stmts.push(typed_stmt);
        }

        // Only the final expression is in tail position (if block is in tail position)
        let (typed_final_expr, block_type) = match final_expr {
            Some(expr) => {
                let typed = self.check_expr_tailrec(expr, in_tail_position)?;
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
                        is_tail_call: false, // Non-tailrec context
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
