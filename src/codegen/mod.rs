pub mod error;

use std::collections::HashMap;

use wasm_encoder::{
    BlockType, CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction,
    Module, TypeSection, ValType,
};

use crate::ast::{BinOp, Literal};
use crate::typechecker::typed_ast::{TypedExpr, TypedExprKind, TypedProgram, TypedStmt, TypedStmtKind};
use crate::typechecker::types::Type;

use self::error::CodeGenError;

/// WASM code generator for Aria programs.
pub struct CodeGen {
    /// Maps function names to their WASM function indices.
    functions: HashMap<String, u32>,
    /// Maps variable names to their local indices (per-function).
    locals: HashMap<String, u32>,
    /// Tracks the next available local index.
    next_local: u32,
    /// Accumulated function types (signatures).
    function_types: Vec<(Vec<ValType>, Vec<ValType>)>,
    /// Maps function type signature to type index (for deduplication).
    type_indices: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    /// Maps function index to type index.
    function_type_indices: Vec<u32>,
    /// Compiled function bodies.
    compiled_functions: Vec<CompiledFunction>,
    /// Function exports (name, function index).
    exports: Vec<(String, u32)>,
    /// Current tailrec context (if compiling a tailrec function).
    tailrec_context: Option<TailrecContext>,
}

/// Context for tail recursion optimization.
struct TailrecContext {
    /// Name of the current tailrec function.
    func_name: String,
    /// Current control flow depth (number of if/block we're nested inside the loop).
    /// Used to compute the correct branch label for `br` back to the loop.
    control_depth: u32,
}

struct CompiledFunction {
    locals: Vec<(u32, ValType)>,
    instructions: Vec<Instruction<'static>>,
}

impl CodeGen {
    pub fn new() -> Self {
        CodeGen {
            functions: HashMap::new(),
            locals: HashMap::new(),
            next_local: 0,
            function_types: Vec::new(),
            type_indices: HashMap::new(),
            function_type_indices: Vec::new(),
            compiled_functions: Vec::new(),
            exports: Vec::new(),
            tailrec_context: None,
        }
    }

    /// Generate WASM bytecode from a typed program.
    pub fn generate(&mut self, program: &TypedProgram) -> Result<Vec<u8>, CodeGenError> {
        // First pass: register all function signatures
        for stmt in &program.stmts {
            if let TypedStmtKind::Fn {
                name,
                params,
                return_ty,
                ..
            } = &stmt.kind
            {
                let func_idx = self.functions.len() as u32;
                self.functions.insert(name.clone(), func_idx);

                let param_types: Vec<ValType> = params
                    .iter()
                    .map(|(_, ty)| self.type_to_valtype(ty))
                    .collect::<Result<_, _>>()?;

                let return_types = self.return_type_to_valtypes(return_ty)?;
                let type_idx = self.get_or_create_type(param_types, return_types);
                self.function_type_indices.push(type_idx);

                self.exports.push((name.clone(), func_idx));
            }
        }

        // Check if we have any top-level non-function statements that need a main
        let has_top_level_code = program.stmts.iter().any(|stmt| {
            !matches!(stmt.kind, TypedStmtKind::Fn { .. })
        });

        if has_top_level_code {
            // Create a main function for top-level code
            let main_idx = self.functions.len() as u32;
            self.functions.insert("main".to_string(), main_idx);

            // main() -> i32 (returns last expression value or 0)
            let type_idx = self.get_or_create_type(vec![], vec![ValType::I32]);
            self.function_type_indices.push(type_idx);

            self.exports.push(("main".to_string(), main_idx));
        }

        // Second pass: compile function bodies
        for stmt in &program.stmts {
            if let TypedStmtKind::Fn {
                name,
                params,
                body,
                is_tailrec,
                ..
            } = &stmt.kind
            {
                self.compile_function(name, params, body, *is_tailrec)?;
            }
        }

        // Compile main function if needed
        if has_top_level_code {
            self.compile_main(program)?;
        }

        // Build the WASM module
        Ok(self.build_module())
    }

    fn get_or_create_type(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        let key = (params.clone(), results.clone());
        if let Some(&idx) = self.type_indices.get(&key) {
            return idx;
        }

        let idx = self.function_types.len() as u32;
        self.function_types.push((params, results));
        self.type_indices.insert(key, idx);
        idx
    }

    fn type_to_valtype(&self, ty: &Type) -> Result<ValType, CodeGenError> {
        match ty {
            Type::Int => Ok(ValType::I32),
            Type::Float => Ok(ValType::F64),
            Type::Bool => Ok(ValType::I32),
            Type::Unit => Err(CodeGenError::UnsupportedType("Unit".to_string())),
            Type::String => Err(CodeGenError::UnsupportedType("String".to_string())),
            Type::Function { .. } => {
                Err(CodeGenError::UnsupportedType("Function".to_string()))
            }
        }
    }

    fn return_type_to_valtypes(&self, ty: &Type) -> Result<Vec<ValType>, CodeGenError> {
        match ty {
            Type::Unit => Ok(vec![]),
            _ => Ok(vec![self.type_to_valtype(ty)?]),
        }
    }

    fn compile_function(
        &mut self,
        name: &str,
        params: &[(String, Type)],
        body: &TypedExpr,
        is_tailrec: bool,
    ) -> Result<(), CodeGenError> {
        // Reset local state for this function
        self.locals.clear();
        self.next_local = 0;

        // Parameters are the first locals
        for (param_name, _) in params {
            let idx = self.next_local;
            self.locals.insert(param_name.clone(), idx);
            self.next_local += 1;
        }

        let mut instructions = if is_tailrec {
            // Set up tailrec context
            self.tailrec_context = Some(TailrecContext {
                func_name: name.to_string(),
                control_depth: 0,
            });

            // Determine block type for the loop based on return type
            let block_type = match &body.ty {
                Type::Unit => BlockType::Empty,
                Type::Int | Type::Bool => BlockType::Result(ValType::I32),
                Type::Float => BlockType::Result(ValType::F64),
                ty => {
                    return Err(CodeGenError::UnsupportedType(format!(
                        "tailrec return type: {}",
                        ty
                    )))
                }
            };

            // Generate: loop $tailrec <body> end
            let mut instrs = vec![Instruction::Loop(block_type)];
            instrs.extend(self.gen_expr(body)?);
            instrs.push(Instruction::End); // end loop

            // Clear tailrec context
            self.tailrec_context = None;

            instrs
        } else {
            // Regular function compilation
            self.gen_expr(body)?
        };

        instructions.push(Instruction::End); // end function

        // Collect locals that were allocated beyond parameters
        let num_params = params.len() as u32;
        let extra_locals = if self.next_local > num_params {
            // For simplicity, declare all extra locals as i32
            // In a more sophisticated implementation, we'd track types
            vec![(self.next_local - num_params, ValType::I32)]
        } else {
            vec![]
        };

        self.compiled_functions.push(CompiledFunction {
            locals: extra_locals,
            instructions,
        });

        Ok(())
    }

    fn compile_main(&mut self, program: &TypedProgram) -> Result<(), CodeGenError> {
        // Reset local state
        self.locals.clear();
        self.next_local = 0;

        let mut instructions = Vec::new();
        let mut last_expr_type = Type::Unit;

        for stmt in &program.stmts {
            match &stmt.kind {
                TypedStmtKind::Fn { .. } => {
                    // Skip function definitions, already compiled
                }
                TypedStmtKind::Let { name, value, .. } => {
                    // Generate value
                    let value_instrs = self.gen_expr(value)?;
                    instructions.extend(value_instrs);

                    // Allocate local
                    let idx = self.next_local;
                    self.locals.insert(name.clone(), idx);
                    self.next_local += 1;

                    // Store in local
                    instructions.push(Instruction::LocalSet(idx));
                    last_expr_type = Type::Unit;
                }
                TypedStmtKind::Expr(expr) => {
                    let expr_instrs = self.gen_expr(expr)?;
                    instructions.extend(expr_instrs);
                    last_expr_type = expr.ty.clone();

                    // If this isn't the last statement, drop the value
                    // We'll handle this more carefully below
                }
            }
        }

        // If the last expression isn't already an i32, we need to handle it
        // For now, if it's Unit, push 0; if it's something else, it should be i32
        if last_expr_type == Type::Unit {
            instructions.push(Instruction::I32Const(0));
        } else if last_expr_type == Type::Float {
            // Convert f64 to i32 by truncation
            instructions.push(Instruction::I32TruncF64S);
        }

        instructions.push(Instruction::End);

        let locals = if self.next_local > 0 {
            vec![(self.next_local, ValType::I32)]
        } else {
            vec![]
        };

        self.compiled_functions.push(CompiledFunction {
            locals,
            instructions,
        });

        Ok(())
    }

    fn gen_expr(&mut self, expr: &TypedExpr) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        match &expr.kind {
            TypedExprKind::Literal(lit) => self.gen_literal(lit),
            TypedExprKind::Ident(name) => self.gen_ident(name),
            TypedExprKind::Binary { op, left, right } => {
                self.gen_binary(*op, left, right, &expr.ty)
            }
            TypedExprKind::Call { callee, args } => self.gen_call(callee, args),
            TypedExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.gen_if(condition, then_branch, else_branch, &expr.ty),
            TypedExprKind::Block { stmts, expr: final_expr } => {
                self.gen_block(stmts, final_expr.as_deref(), &expr.ty)
            }
        }
    }

    fn gen_literal(&self, lit: &Literal) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        match lit {
            Literal::Integer(n) => Ok(vec![Instruction::I32Const(*n as i32)]),
            Literal::Float(f) => Ok(vec![Instruction::F64Const(*f)]),
            Literal::Bool(b) => Ok(vec![Instruction::I32Const(if *b { 1 } else { 0 })]),
            Literal::String(_) => {
                Err(CodeGenError::UnsupportedType("String literals".to_string()))
            }
        }
    }

    fn gen_ident(&self, name: &str) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        match self.locals.get(name) {
            Some(&idx) => Ok(vec![Instruction::LocalGet(idx)]),
            None => Err(CodeGenError::UndefinedVariable(name.to_string())),
        }
    }

    fn gen_binary(
        &mut self,
        op: BinOp,
        left: &TypedExpr,
        right: &TypedExpr,
        _result_ty: &Type,
    ) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        let mut instrs = self.gen_expr(left)?;
        instrs.extend(self.gen_expr(right)?);

        // Determine if we're doing int or float operations based on operand type
        let is_float = matches!(left.ty, Type::Float);

        let op_instr = if is_float {
            match op {
                BinOp::Add => Instruction::F64Add,
                BinOp::Sub => Instruction::F64Sub,
                BinOp::Mul => Instruction::F64Mul,
                BinOp::Div => Instruction::F64Div,
                BinOp::Mod => {
                    return Err(CodeGenError::UnsupportedFeature(
                        "modulo on floats".to_string(),
                    ))
                }
                BinOp::Eq => Instruction::F64Eq,
                BinOp::Ne => Instruction::F64Ne,
                BinOp::Lt => Instruction::F64Lt,
                BinOp::Gt => Instruction::F64Gt,
                BinOp::Le => Instruction::F64Le,
                BinOp::Ge => Instruction::F64Ge,
            }
        } else {
            match op {
                BinOp::Add => Instruction::I32Add,
                BinOp::Sub => Instruction::I32Sub,
                BinOp::Mul => Instruction::I32Mul,
                BinOp::Div => Instruction::I32DivS,
                BinOp::Mod => Instruction::I32RemS,
                BinOp::Eq => Instruction::I32Eq,
                BinOp::Ne => Instruction::I32Ne,
                BinOp::Lt => Instruction::I32LtS,
                BinOp::Gt => Instruction::I32GtS,
                BinOp::Le => Instruction::I32LeS,
                BinOp::Ge => Instruction::I32GeS,
            }
        };

        instrs.push(op_instr);
        Ok(instrs)
    }

    fn gen_call(
        &mut self,
        callee: &str,
        args: &[TypedExpr],
    ) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        // Check if this is a tail recursive call
        let is_tail_recursive = self
            .tailrec_context
            .as_ref()
            .map(|ctx| ctx.func_name == callee)
            .unwrap_or(false);

        if is_tail_recursive {
            // Tail recursive call: update parameters and branch back to loop start
            return self.gen_tail_call(args);
        }

        // Regular function call
        let func_idx = self
            .functions
            .get(callee)
            .copied()
            .ok_or_else(|| CodeGenError::UndefinedFunction(callee.to_string()))?;

        let mut instrs = Vec::new();

        // Generate arguments
        for arg in args {
            instrs.extend(self.gen_expr(arg)?);
        }

        instrs.push(Instruction::Call(func_idx));
        Ok(instrs)
    }

    /// Generate code for a tail recursive call (loop back).
    fn gen_tail_call(&mut self, args: &[TypedExpr]) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        let control_depth = self
            .tailrec_context
            .as_ref()
            .map(|ctx| ctx.control_depth)
            .unwrap_or(0);

        let mut instrs = Vec::new();

        // We need to evaluate all arguments first, then store them.
        // This handles cases like `tailrec fn f(a, b) { f(b, a) }`
        // where we can't just store a=b, b=a directly.

        // Strategy: use temporary locals
        // 1. Evaluate each arg and store in a temp local
        // 2. Load temps into parameter locals

        let mut temp_locals = Vec::new();

        // Step 1: Evaluate args and store in temps
        for arg in args {
            instrs.extend(self.gen_expr(arg)?);
            let temp_idx = self.next_local;
            self.next_local += 1;
            temp_locals.push(temp_idx);
            instrs.push(Instruction::LocalSet(temp_idx));
        }

        // Step 2: Load temps into parameter locals
        for (i, &temp_idx) in temp_locals.iter().enumerate() {
            instrs.push(Instruction::LocalGet(temp_idx));
            instrs.push(Instruction::LocalSet(i as u32));
        }

        // Step 3: Branch back to loop start
        // The branch label is the control_depth (number of if/block structures
        // we're nested inside, as we need to target the outer loop)
        instrs.push(Instruction::Br(control_depth));

        Ok(instrs)
    }

    fn gen_if(
        &mut self,
        condition: &TypedExpr,
        then_branch: &TypedExpr,
        else_branch: &TypedExpr,
        result_ty: &Type,
    ) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        let mut instrs = self.gen_expr(condition)?;

        // Determine block type based on result type
        let block_type = match result_ty {
            Type::Unit => BlockType::Empty,
            Type::Int | Type::Bool => BlockType::Result(ValType::I32),
            Type::Float => BlockType::Result(ValType::F64),
            _ => {
                return Err(CodeGenError::UnsupportedType(format!(
                    "if expression result type: {}",
                    result_ty
                )))
            }
        };

        // Track control flow depth for tailrec
        if let Some(ref mut ctx) = self.tailrec_context {
            ctx.control_depth += 1;
        }

        instrs.push(Instruction::If(block_type));
        instrs.extend(self.gen_expr(then_branch)?);
        instrs.push(Instruction::Else);
        instrs.extend(self.gen_expr(else_branch)?);
        instrs.push(Instruction::End);

        // Restore control flow depth
        if let Some(ref mut ctx) = self.tailrec_context {
            ctx.control_depth -= 1;
        }

        Ok(instrs)
    }

    fn gen_block(
        &mut self,
        stmts: &[TypedStmt],
        final_expr: Option<&TypedExpr>,
        _result_ty: &Type,
    ) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        let mut instrs = Vec::new();

        for stmt in stmts {
            instrs.extend(self.gen_stmt(stmt)?);
        }

        if let Some(expr) = final_expr {
            instrs.extend(self.gen_expr(expr)?);
        }

        Ok(instrs)
    }

    fn gen_stmt(&mut self, stmt: &TypedStmt) -> Result<Vec<Instruction<'static>>, CodeGenError> {
        match &stmt.kind {
            TypedStmtKind::Let { name, value, .. } => {
                let mut instrs = self.gen_expr(value)?;

                // Allocate local
                let idx = self.next_local;
                self.locals.insert(name.clone(), idx);
                self.next_local += 1;

                instrs.push(Instruction::LocalSet(idx));
                Ok(instrs)
            }
            TypedStmtKind::Fn { .. } => {
                // Function definitions inside blocks are not supported
                Err(CodeGenError::UnsupportedFeature(
                    "nested function definitions".to_string(),
                ))
            }
            TypedStmtKind::Expr(expr) => {
                let mut instrs = self.gen_expr(expr)?;

                // Drop the value if it's not Unit
                if expr.ty != Type::Unit {
                    instrs.push(Instruction::Drop);
                }

                Ok(instrs)
            }
        }
    }

    fn build_module(&self) -> Vec<u8> {
        let mut module = Module::new();

        // Type section
        let mut types = TypeSection::new();
        for (params, results) in &self.function_types {
            types.ty().function(params.iter().copied(), results.iter().copied());
        }
        module.section(&types);

        // Function section (maps function index to type index)
        let mut functions = FunctionSection::new();
        for &type_idx in &self.function_type_indices {
            functions.function(type_idx);
        }
        module.section(&functions);

        // Export section
        let mut exports = ExportSection::new();
        for (name, func_idx) in &self.exports {
            exports.export(name, ExportKind::Func, *func_idx);
        }
        module.section(&exports);

        // Code section
        let mut code = CodeSection::new();
        for compiled in &self.compiled_functions {
            let mut func = Function::new(compiled.locals.clone());
            for instr in &compiled.instructions {
                func.instruction(instr);
            }
            code.function(&func);
        }
        module.section(&code);

        module.finish()
    }
}

impl Default for CodeGen {
    fn default() -> Self {
        Self::new()
    }
}
