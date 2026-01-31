pub mod env;
pub mod error;
pub mod typed_ast;
pub mod types;

#[cfg(test)]
mod tests;

use crate::ast::{BinOp, Expr, Literal, MatchArm, Pattern, Program, Spanned, Stmt};

use self::env::Env;
use self::error::TypeError;
use self::typed_ast::{
    MonomorphizationRequest, TypedExpr, TypedExprKind, TypedMatchArm, TypedPattern,
    TypedPatternKind, TypedProgram, TypedStmt, TypedStmtKind,
};
use self::types::{EnumVariantDef, Type};
use std::collections::HashSet;

/// Bidirectional type checker for Aria programs.
pub struct TypeChecker {
    env: Env,
    errors: Vec<TypeError>,
    /// Current function context for tailrec checking
    current_function: Option<CurrentFunction>,
    /// Monomorphization requests generated during type checking
    monomorphizations: Vec<MonomorphizationRequest>,
    /// Track which monomorphizations have already been generated
    generated_monomorphizations: HashSet<String>,
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
            monomorphizations: Vec::new(),
            generated_monomorphizations: HashSet::new(),
        }
    }

    /// Type check a program, returning either a typed AST or accumulated errors.
    pub fn check(&mut self, program: &Program) -> Result<TypedProgram, Vec<TypeError>> {
        // First pass: register all enum definitions
        for stmt in &program.stmts {
            if let Stmt::Enum { name, variants, .. } = &stmt.node {
                let typed_variants: Vec<EnumVariantDef> = variants
                    .iter()
                    .map(|v| {
                        let payload = v.node.payload.as_ref().map(|ty| {
                            Box::new(Type::from_ast(&ty.node))
                        });
                        EnumVariantDef {
                            name: v.node.name.clone(),
                            payload,
                        }
                    })
                    .collect();

                let enum_type = Type::Enum {
                    name: name.clone(),
                    variants: typed_variants,
                };

                if !self.env.define_enum(name.clone(), enum_type) {
                    self.errors.push(TypeError::duplicate_definition(
                        name.clone(),
                        stmt.span.clone(),
                    ));
                }
            }
        }

        // Second pass: register all top-level function signatures
        for stmt in &program.stmts {
            if let Stmt::Fn {
                name,
                type_params,
                params,
                return_ty,
                ..
            } = &stmt.node
            {
                if type_params.is_empty() {
                    // Non-generic function: register its type directly
                    let param_types: Vec<Type> = params
                        .iter()
                        .map(|p| self.env.resolve_type(&Type::from_ast(&p.node.ty.node)))
                        .collect();
                    let ret = self.env.resolve_type(&Type::from_ast(&return_ty.node));
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
                } else {
                    // Generic function: store definition for later instantiation
                    if !self.env.define_generic_function(name.clone(), type_params.clone(), stmt.clone()) {
                        self.errors.push(TypeError::duplicate_definition(
                            name.clone(),
                            stmt.span.clone(),
                        ));
                    }
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
            Ok(TypedProgram::with_monomorphizations(
                typed_stmts,
                std::mem::take(&mut self.monomorphizations),
            ))
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<TypedStmt, TypeError> {
        match &stmt.node {
            Stmt::Let { name, ty, value } => self.check_let_stmt(name, ty, value, stmt.span.clone()),
            Stmt::Fn {
                name,
                type_params,
                params,
                return_ty,
                body,
                is_tailrec,
            } => self.check_fn_stmt(name, type_params, params, return_ty, body, *is_tailrec, stmt.span.clone()),
            Stmt::Expr(expr) => {
                let typed_expr = self.check_expr(expr)?;
                Ok(TypedStmt::new(TypedStmtKind::Expr(typed_expr), stmt.span.clone()))
            }
            Stmt::Enum { name, type_params: _, variants } => {
                // Enum was already registered in the first pass
                // Just create the typed statement
                // TODO: Handle generic enums
                let typed_variants: Vec<EnumVariantDef> = variants
                    .iter()
                    .map(|v| {
                        let payload = v.node.payload.as_ref().map(|ty| {
                            Box::new(self.env.resolve_type(&Type::from_ast(&ty.node)))
                        });
                        EnumVariantDef {
                            name: v.node.name.clone(),
                            payload,
                        }
                    })
                    .collect();

                Ok(TypedStmt::new(
                    TypedStmtKind::Enum {
                        name: name.clone(),
                        variants: typed_variants,
                    },
                    stmt.span.clone(),
                ))
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
        type_params: &[String],
        params: &[Spanned<crate::ast::Param>],
        return_ty: &Spanned<crate::ast::Type>,
        body: &Spanned<Expr>,
        is_tailrec: bool,
        span: logos::Span,
    ) -> Result<TypedStmt, TypeError> {
        // For generic functions, we skip type checking the body until instantiation
        if !type_params.is_empty() {
            // Generic functions are stored but not compiled until instantiated
            // Return a placeholder typed statement
            // The actual type checking happens when the function is called with concrete types
            return Ok(TypedStmt::new(
                TypedStmtKind::GenericFn {
                    name: name.to_string(),
                    type_params: type_params.to_vec(),
                },
                span,
            ));
        }

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
                // First check if it's a variable
                if let Some(ty) = self.env.lookup(name) {
                    return Ok(TypedExpr::new(
                        TypedExprKind::Ident(name.clone()),
                        ty.clone(),
                        expr.span.clone(),
                    ));
                }
                // Check if it's a unit enum variant (no payload)
                if let Some((enum_name, variant_idx, variant_def)) = self.env.lookup_variant(name) {
                    if variant_def.payload.is_some() {
                        return Err(TypeError::missing_variant_payload(name.clone(), expr.span.clone()));
                    }
                    let enum_type = self.env.lookup_enum(enum_name).unwrap().clone();
                    return Ok(TypedExpr::new(
                        TypedExprKind::EnumVariant {
                            enum_name: enum_name.to_string(),
                            variant_name: name.clone(),
                            variant_index: variant_idx,
                            payload: None,
                        },
                        enum_type,
                        expr.span.clone(),
                    ));
                }
                Err(TypeError::undefined_variable(name.clone(), expr.span.clone()))
            }
            Expr::Binary { op, left, right } => self.check_binary(*op, left, right, expr.span.clone()),
            Expr::Call { callee, type_args, args } => self.check_call(callee, type_args.as_ref(), args, expr.span.clone()),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => self.check_if(condition, then_branch, else_branch, expr.span.clone()),
            Expr::Block { stmts, expr: final_expr } => {
                self.check_block(stmts, final_expr, expr.span.clone())
            }
            Expr::Match { expr: match_expr, arms } => {
                self.check_match(match_expr, arms, expr.span.clone())
            }
            Expr::EnumVariant { variant, payload } => {
                self.check_enum_variant(variant, payload, expr.span.clone())
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
                // First check if it's a variable
                if let Some(ty) = self.env.lookup(name) {
                    return Ok(TypedExpr::new(
                        TypedExprKind::Ident(name.clone()),
                        ty.clone(),
                        expr.span.clone(),
                    ));
                }
                // Check if it's a unit enum variant (no payload)
                if let Some((enum_name, variant_idx, variant_def)) = self.env.lookup_variant(name) {
                    if variant_def.payload.is_some() {
                        return Err(TypeError::missing_variant_payload(name.clone(), expr.span.clone()));
                    }
                    let enum_type = self.env.lookup_enum(enum_name).unwrap().clone();
                    return Ok(TypedExpr::new(
                        TypedExprKind::EnumVariant {
                            enum_name: enum_name.to_string(),
                            variant_name: name.clone(),
                            variant_index: variant_idx,
                            payload: None,
                        },
                        enum_type,
                        expr.span.clone(),
                    ));
                }
                Err(TypeError::undefined_variable(name.clone(), expr.span.clone()))
            }
            Expr::Binary { op, left, right } => {
                // Binary operations: neither operand is in tail position
                self.check_binary_tailrec(*op, left, right, expr.span.clone())
            }
            Expr::Call { callee, type_args, args } => {
                self.check_call_tailrec(callee, type_args.as_ref(), args, expr.span.clone(), in_tail_position)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => self.check_if_tailrec(condition, then_branch, else_branch, expr.span.clone(), in_tail_position),
            Expr::Block { stmts, expr: final_expr } => {
                self.check_block_tailrec(stmts, final_expr, expr.span.clone(), in_tail_position)
            }
            // For now, match expressions in tailrec context use the non-tailrec path
            // A more advanced implementation could track tail positions in match arms
            Expr::Match { expr: match_expr, arms } => {
                self.check_match(match_expr, arms, expr.span.clone())
            }
            Expr::EnumVariant { variant, payload } => {
                self.check_enum_variant(variant, payload, expr.span.clone())
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
        type_args: Option<&Vec<Spanned<crate::ast::Type>>>,
        args: &[Spanned<Expr>],
        span: logos::Span,
        in_tail_position: bool,
    ) -> Result<TypedExpr, TypeError> {
        // Check if this is a generic function call
        if let Some(generic_def) = self.env.lookup_generic_function(callee) {
            let generic_def = generic_def.clone();
            // For generic calls in tailrec context, use the non-tailrec path for simplicity
            return self.check_generic_call(callee, &generic_def, type_args, args, span);
        }

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
        type_args: Option<&Vec<Spanned<crate::ast::Type>>>,
        args: &[Spanned<Expr>],
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        // Check if this is a generic function call
        if let Some(generic_def) = self.env.lookup_generic_function(callee) {
            let generic_def = generic_def.clone();
            return self.check_generic_call(callee, &generic_def, type_args, args, span);
        }

        // First check if callee is a regular (non-generic) function
        if let Some(fn_type) = self.env.lookup(callee) {
            let fn_type = fn_type.clone();
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

                    return Ok(TypedExpr::new(
                        TypedExprKind::Call {
                            callee: callee.to_string(),
                            args: typed_args,
                            is_tail_call: false, // Non-tailrec context
                        },
                        *ret,
                        span,
                    ));
                }
                other => return Err(TypeError::not_callable(other, span)),
            }
        }

        // Check if callee is an enum variant with payload
        if let Some((enum_name, variant_idx, variant_def)) = self.env.lookup_variant(callee) {
            let enum_name = enum_name.to_string();
            let variant_name = callee.to_string();
            let variant_def = variant_def.clone();

            match &variant_def.payload {
                Some(payload_ty) => {
                    // Variant expects exactly one argument
                    if args.len() != 1 {
                        return Err(TypeError::arity_mismatch(1, args.len(), span));
                    }

                    let payload_ty = self.env.resolve_type(payload_ty);
                    let typed_arg = self.check_expr_expecting(&args[0], &payload_ty)?;
                    let enum_type = self.env.lookup_enum(&enum_name).unwrap().clone();

                    return Ok(TypedExpr::new(
                        TypedExprKind::EnumVariant {
                            enum_name,
                            variant_name,
                            variant_index: variant_idx,
                            payload: Some(Box::new(typed_arg)),
                        },
                        enum_type,
                        span,
                    ));
                }
                None => {
                    // Unit variant called with arguments
                    return Err(TypeError::unexpected_variant_payload(variant_name, span));
                }
            }
        }

        Err(TypeError::undefined_function(callee.to_string(), span))
    }

    /// Check a call to a generic function, instantiating it with concrete types.
    fn check_generic_call(
        &mut self,
        callee: &str,
        generic_def: &env::GenericFunctionDef,
        type_args: Option<&Vec<Spanned<crate::ast::Type>>>,
        args: &[Spanned<Expr>],
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        // Get type arguments (either explicit or inferred)
        let concrete_types: Vec<Type> = match type_args {
            Some(targs) => targs.iter().map(|t| Type::from_ast(&t.node)).collect(),
            None => {
                // Type inference: for now, require explicit type arguments
                return Err(TypeError::type_inference_failed(
                    callee.to_string(),
                    generic_def.type_params.clone(),
                    span,
                ));
            }
        };

        // Check that we have the right number of type arguments
        if concrete_types.len() != generic_def.type_params.len() {
            return Err(TypeError::wrong_number_of_type_args(
                callee.to_string(),
                generic_def.type_params.len(),
                concrete_types.len(),
                span,
            ));
        }

        // Create a type substitution map
        let mut type_subst: std::collections::HashMap<String, Type> = std::collections::HashMap::new();
        for (param, ty) in generic_def.type_params.iter().zip(concrete_types.iter()) {
            type_subst.insert(param.clone(), ty.clone());
        }

        // Get the function parameters and return type from the AST
        let (params, return_ty) = match &generic_def.ast.node {
            Stmt::Fn { params, return_ty, .. } => (params, return_ty),
            _ => unreachable!("generic_def should always contain a Fn statement"),
        };

        // Substitute type parameters in parameter types
        let param_types: Vec<Type> = params
            .iter()
            .map(|p| {
                let ty = Type::from_ast(&p.node.ty.node);
                self.substitute_types(&ty, &type_subst)
            })
            .collect();

        // Substitute type parameters in return type
        let ret_type = self.substitute_types(&Type::from_ast(&return_ty.node), &type_subst);

        // Check arity
        if args.len() != param_types.len() {
            return Err(TypeError::arity_mismatch(param_types.len(), args.len(), span));
        }

        // Check each argument against its expected type
        let mut typed_args = Vec::new();
        for (arg, expected_ty) in args.iter().zip(param_types.iter()) {
            let typed_arg = self.check_expr_expecting(arg, expected_ty)?;
            typed_args.push(typed_arg);
        }

        // Generate a monomorphized function name
        let mono_name = self.mangle_generic_name(callee, &concrete_types);

        // Generate monomorphization request if not already generated
        if !self.generated_monomorphizations.contains(&mono_name) {
            self.generated_monomorphizations.insert(mono_name.clone());

            // Get the function body and is_tailrec from the AST
            let (body, is_tailrec) = match &generic_def.ast.node {
                Stmt::Fn { body, is_tailrec, .. } => (body, *is_tailrec),
                _ => unreachable!(),
            };

            // Type check the body with type parameters bound to concrete types
            self.env.push_scope();

            // Bind parameters with concrete types
            let mut typed_params = Vec::new();
            for (param, concrete_ty) in params.iter().zip(param_types.iter()) {
                self.env.define(param.node.name.clone(), concrete_ty.clone());
                typed_params.push((param.node.name.clone(), concrete_ty.clone()));
            }

            // Type check the body
            let typed_body = self.check_expr_expecting(body, &ret_type)?;

            self.env.pop_scope();

            // Create the monomorphization request
            let request = MonomorphizationRequest {
                mangled_name: mono_name.clone(),
                original_name: callee.to_string(),
                params: typed_params,
                return_ty: ret_type.clone(),
                body: typed_body,
                is_tailrec,
            };
            self.monomorphizations.push(request);
        }

        Ok(TypedExpr::new(
            TypedExprKind::Call {
                callee: mono_name,
                args: typed_args,
                is_tail_call: false,
            },
            ret_type,
            span,
        ))
    }

    /// Substitute type variables with concrete types.
    fn substitute_types(&self, ty: &Type, subst: &std::collections::HashMap<String, Type>) -> Type {
        match ty {
            Type::Enum { name, variants } if variants.is_empty() => {
                // This might be a type variable
                if let Some(concrete) = subst.get(name) {
                    concrete.clone()
                } else {
                    // Try to resolve as enum
                    self.env.resolve_type(ty)
                }
            }
            Type::TypeVar(name) => {
                subst.get(name).cloned().unwrap_or_else(|| ty.clone())
            }
            Type::Function { params, ret } => Type::Function {
                params: params.iter().map(|p| self.substitute_types(p, subst)).collect(),
                ret: Box::new(self.substitute_types(ret, subst)),
            },
            Type::GenericInstance { name, type_args } => Type::GenericInstance {
                name: name.clone(),
                type_args: type_args.iter().map(|t| self.substitute_types(t, subst)).collect(),
            },
            _ => ty.clone(),
        }
    }

    /// Generate a mangled name for a monomorphized generic function.
    fn mangle_generic_name(&self, base_name: &str, type_args: &[Type]) -> String {
        let type_names: Vec<String> = type_args.iter().map(|t| t.display_name()).collect();
        format!("{}${}", base_name, type_names.join("$"))
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

    /// Check a match expression
    fn check_match(
        &mut self,
        expr: &Spanned<Expr>,
        arms: &[Spanned<MatchArm>],
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        let typed_expr = self.check_expr(expr)?;
        let match_type = typed_expr.ty.clone();

        if arms.is_empty() {
            return Err(TypeError::non_exhaustive_match(vec!["any".to_string()], span));
        }

        let mut typed_arms = Vec::new();
        let mut result_type: Option<Type> = None;
        let mut covered_variants: Vec<String> = Vec::new();

        for arm in arms {
            self.env.push_scope();

            // Check the pattern against the match expression type
            let typed_pattern = self.check_pattern(&arm.node.pattern, &match_type)?;

            // Track covered variants for exhaustiveness checking
            if let TypedPatternKind::Variant { variant_name, .. } = &typed_pattern.kind {
                if !covered_variants.contains(variant_name) {
                    covered_variants.push(variant_name.clone());
                }
            } else if let TypedPatternKind::Wildcard = &typed_pattern.kind {
                // Wildcard covers all remaining variants
                if let Type::Enum { variants, .. } = &match_type {
                    for v in variants {
                        if !covered_variants.contains(&v.name) {
                            covered_variants.push(v.name.clone());
                        }
                    }
                }
            } else if let TypedPatternKind::Ident(_) = &typed_pattern.kind {
                // Variable binding covers all remaining variants (acts like wildcard)
                if let Type::Enum { variants, .. } = &match_type {
                    for v in variants {
                        if !covered_variants.contains(&v.name) {
                            covered_variants.push(v.name.clone());
                        }
                    }
                }
            }

            // Check the body expression
            let typed_body = self.check_expr(&arm.node.body)?;

            // All arms must have the same type
            match &result_type {
                None => {
                    result_type = Some(typed_body.ty.clone());
                }
                Some(first_ty) => {
                    if typed_body.ty != *first_ty {
                        self.env.pop_scope();
                        return Err(TypeError::match_arm_mismatch(
                            first_ty.clone(),
                            typed_body.ty.clone(),
                            arm.span.clone(),
                        ));
                    }
                }
            }

            self.env.pop_scope();

            typed_arms.push(TypedMatchArm {
                pattern: typed_pattern,
                body: typed_body,
            });
        }

        // Check exhaustiveness for enum types
        if let Type::Enum { variants, .. } = &match_type {
            let missing: Vec<String> = variants
                .iter()
                .filter(|v| !covered_variants.contains(&v.name))
                .map(|v| v.name.clone())
                .collect();

            if !missing.is_empty() {
                return Err(TypeError::non_exhaustive_match(missing, span));
            }
        }

        let final_type = result_type.unwrap_or(Type::Unit);

        Ok(TypedExpr::new(
            TypedExprKind::Match {
                expr: Box::new(typed_expr),
                arms: typed_arms,
            },
            final_type,
            span,
        ))
    }

    /// Check an enum variant constructor expression
    fn check_enum_variant(
        &mut self,
        variant_name: &str,
        payload: &Option<Box<Spanned<Expr>>>,
        span: logos::Span,
    ) -> Result<TypedExpr, TypeError> {
        // Look up the variant
        let (enum_name, variant_idx, variant_def) = match self.env.lookup_variant(variant_name) {
            Some((en, idx, vd)) => (en.to_string(), idx, vd.clone()),
            None => {
                return Err(TypeError::undefined_variant(
                    "unknown".to_string(),
                    variant_name.to_string(),
                    span,
                ));
            }
        };

        let enum_type = self.env.lookup_enum(&enum_name).unwrap().clone();

        match (&variant_def.payload, payload) {
            (Some(expected_ty), Some(payload_expr)) => {
                let expected_ty = self.env.resolve_type(expected_ty);
                let typed_payload = self.check_expr_expecting(payload_expr, &expected_ty)?;
                Ok(TypedExpr::new(
                    TypedExprKind::EnumVariant {
                        enum_name,
                        variant_name: variant_name.to_string(),
                        variant_index: variant_idx,
                        payload: Some(Box::new(typed_payload)),
                    },
                    enum_type,
                    span,
                ))
            }
            (None, None) => {
                Ok(TypedExpr::new(
                    TypedExprKind::EnumVariant {
                        enum_name,
                        variant_name: variant_name.to_string(),
                        variant_index: variant_idx,
                        payload: None,
                    },
                    enum_type,
                    span,
                ))
            }
            (Some(_), None) => {
                Err(TypeError::missing_variant_payload(variant_name.to_string(), span))
            }
            (None, Some(_)) => {
                Err(TypeError::unexpected_variant_payload(variant_name.to_string(), span))
            }
        }
    }

    /// Check a pattern against an expected type
    fn check_pattern(
        &mut self,
        pattern: &Spanned<Pattern>,
        expected_type: &Type,
    ) -> Result<TypedPattern, TypeError> {
        // Resolve the expected type to its full definition
        let expected_type = self.env.resolve_type(expected_type);

        match &pattern.node {
            Pattern::Wildcard => {
                Ok(TypedPattern::new(
                    TypedPatternKind::Wildcard,
                    expected_type.clone(),
                    pattern.span.clone(),
                ))
            }
            Pattern::Literal(lit) => {
                let lit_type = self.type_of_literal(lit);
                if lit_type != expected_type {
                    return Err(TypeError::pattern_type_mismatch(
                        expected_type.clone(),
                        lit_type,
                        pattern.span.clone(),
                    ));
                }
                Ok(TypedPattern::new(
                    TypedPatternKind::Literal(lit.clone()),
                    expected_type.clone(),
                    pattern.span.clone(),
                ))
            }
            Pattern::Ident(name) => {
                // Variable binding - binds the matched value to a new variable
                self.env.define(name.clone(), expected_type.clone());
                Ok(TypedPattern::new(
                    TypedPatternKind::Ident(name.clone()),
                    expected_type.clone(),
                    pattern.span.clone(),
                ))
            }
            Pattern::Variant { name, payload } => {
                // Check that we're matching against an enum type
                let (enum_name, variants) = match &expected_type {
                    Type::Enum { name, variants } => (name.clone(), variants.clone()),
                    _ => {
                        return Err(TypeError::pattern_type_mismatch(
                            expected_type,
                            Type::Enum {
                                name: "enum".to_string(),
                                variants: vec![],
                            },
                            pattern.span.clone(),
                        ));
                    }
                };

                // Find the variant
                let (variant_idx, variant_def) = variants
                    .iter()
                    .enumerate()
                    .find(|(_, v)| v.name == *name)
                    .map(|(idx, v)| (idx, v.clone()))
                    .ok_or_else(|| {
                        TypeError::undefined_variant(enum_name.clone(), name.clone(), pattern.span.clone())
                    })?;

                // Check payload
                let typed_payload = match (&variant_def.payload, payload) {
                    (Some(payload_ty), Some(inner_pattern)) => {
                        let payload_ty = self.env.resolve_type(payload_ty);
                        let typed_inner = self.check_pattern(inner_pattern, &payload_ty)?;
                        Some(Box::new(typed_inner))
                    }
                    (None, None) => None,
                    (Some(_), None) => {
                        return Err(TypeError::missing_variant_payload(name.clone(), pattern.span.clone()));
                    }
                    (None, Some(_)) => {
                        return Err(TypeError::unexpected_variant_payload(name.clone(), pattern.span.clone()));
                    }
                };

                Ok(TypedPattern::new(
                    TypedPatternKind::Variant {
                        enum_name,
                        variant_name: name.clone(),
                        variant_index: variant_idx,
                        payload: typed_payload,
                    },
                    expected_type.clone(),
                    pattern.span.clone(),
                ))
            }
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
