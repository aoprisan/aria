use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::ast::{Program, Stmt};
use crate::parser::Parser;
use crate::typechecker::TypeChecker;

use super::errors::{report_parse_error, report_type_errors};

const HELP_TEXT: &str = r#"Aria REPL Commands:
  :help     Show this help message
  :quit     Exit the REPL
  :clear    Clear all accumulated definitions
  :env      Show defined variables and functions

Enter Aria code to evaluate. Definitions (let, fn) persist across inputs.
"#;

pub fn run() -> Result<(), i32> {
    println!("Aria REPL (type :help for commands, :quit to exit)");

    let mut editor = match DefaultEditor::new() {
        Ok(e) => e,
        Err(e) => {
            eprintln!("error: failed to initialize REPL: {}", e);
            return Err(1);
        }
    };

    let mut accumulated_source = String::new();
    let mut accumulated_program = Program { stmts: Vec::new() };

    loop {
        let prompt = if accumulated_source.is_empty() {
            ">> "
        } else {
            ".. "
        };

        let line = match editor.readline(prompt) {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) => {
                // Ctrl-C: clear current input
                accumulated_source.clear();
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                // Ctrl-D: exit
                println!();
                break;
            }
            Err(e) => {
                eprintln!("error: {}", e);
                return Err(1);
            }
        };

        let trimmed = line.trim();

        // Handle commands
        if trimmed.starts_with(':') {
            match trimmed {
                ":help" | ":h" | ":?" => {
                    print!("{}", HELP_TEXT);
                }
                ":quit" | ":q" | ":exit" => {
                    break;
                }
                ":clear" => {
                    accumulated_source.clear();
                    accumulated_program = Program { stmts: Vec::new() };
                    println!("Cleared all definitions");
                }
                ":env" => {
                    print_env(&accumulated_program);
                }
                _ => {
                    eprintln!("Unknown command: {} (try :help)", trimmed);
                }
            }
            continue;
        }

        // Skip empty lines
        if trimmed.is_empty() {
            continue;
        }

        let _ = editor.add_history_entry(&line);

        // Accumulate source
        if !accumulated_source.is_empty() {
            accumulated_source.push('\n');
        }
        accumulated_source.push_str(&line);

        // Try to parse
        let full_source = format_full_source(&accumulated_program, &accumulated_source);
        let mut parser = Parser::new(&full_source);

        let ast = match parser.parse_program() {
            Ok(ast) => ast,
            Err(e) => {
                // Check if this looks like an incomplete input
                if is_incomplete_input(&accumulated_source) {
                    // Wait for more input
                    continue;
                }
                report_parse_error("<repl>", &full_source, &e);
                accumulated_source.clear();
                continue;
            }
        };

        // Type check
        let mut typechecker = TypeChecker::new();
        let typed_ast = match typechecker.check(&ast) {
            Ok(typed) => typed,
            Err(errors) => {
                report_type_errors("<repl>", &full_source, &errors);
                accumulated_source.clear();
                continue;
            }
        };

        // Find the new statements (those after the accumulated ones)
        let num_old = accumulated_program.stmts.len();
        let new_stmts = &ast.stmts[num_old..];
        let new_typed_stmts = &typed_ast.stmts[num_old..];

        // Display results for new statements
        for (stmt, typed_stmt) in new_stmts.iter().zip(new_typed_stmts.iter()) {
            match &stmt.node {
                Stmt::Let { name, .. } => {
                    let ty = match &typed_stmt.kind {
                        crate::typechecker::typed_ast::TypedStmtKind::Let { ty, .. } => ty.clone(),
                        _ => unreachable!(),
                    };
                    println!("{} : {}", name, ty);
                }
                Stmt::Fn {
                    name,
                    params,
                    return_ty,
                    ..
                } => {
                    let params_str: Vec<String> = params
                        .iter()
                        .map(|p| format!("{}: {}", p.node.name, type_to_string(&p.node.ty.node)))
                        .collect();
                    println!(
                        "{} : fn({}) -> {}",
                        name,
                        params_str.join(", "),
                        type_to_string(&return_ty.node)
                    );
                }
                Stmt::Expr(_) => {
                    let ty = match &typed_stmt.kind {
                        crate::typechecker::typed_ast::TypedStmtKind::Expr(expr) => &expr.ty,
                        _ => unreachable!(),
                    };
                    // For expression statements, show the type
                    // In a full implementation, we'd also show the value
                    println!("_ : {}", ty);
                }
                Stmt::Enum { name, variants, .. } => {
                    let variant_names: Vec<String> = variants
                        .iter()
                        .map(|v| {
                            if v.node.payload.is_some() {
                                format!("{}(...)", v.node.name)
                            } else {
                                v.node.name.clone()
                            }
                        })
                        .collect();
                    println!("enum {} {{ {} }}", name, variant_names.join(", "));
                }
            }
        }

        // Persist only definitions (let, fn, enum), not expression statements
        for stmt in new_stmts {
            match &stmt.node {
                Stmt::Let { .. } | Stmt::Fn { .. } | Stmt::Enum { .. } => {
                    accumulated_program.stmts.push(stmt.clone());
                }
                Stmt::Expr(_) => {
                    // Don't persist expression statements
                }
            }
        }

        // Clear current input for next round
        accumulated_source.clear();
    }

    Ok(())
}

fn format_full_source(accumulated: &Program, new_source: &str) -> String {
    let mut full = String::new();

    // Rebuild source from accumulated statements
    for stmt in &accumulated.stmts {
        full.push_str(&format_stmt(&stmt.node));
        full.push('\n');
    }

    full.push_str(new_source);
    full
}

fn format_stmt(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Let { name, ty, value } => {
            let ty_str = ty
                .as_ref()
                .map(|t| format!(": {}", type_to_string(&t.node)))
                .unwrap_or_default();
            format!("let {}{} = {};", name, ty_str, format_expr(&value.node))
        }
        Stmt::Fn {
            name,
            params,
            return_ty,
            body,
            is_tailrec,
            ..
        } => {
            let params_str: Vec<String> = params
                .iter()
                .map(|p| format!("{}: {}", p.node.name, type_to_string(&p.node.ty.node)))
                .collect();
            let prefix = if *is_tailrec { "tailrec fn" } else { "fn" };
            format!(
                "{} {}({}) -> {} {}",
                prefix,
                name,
                params_str.join(", "),
                type_to_string(&return_ty.node),
                format_expr(&body.node)
            )
        }
        Stmt::Expr(expr) => {
            format!("{};", format_expr(&expr.node))
        }
        Stmt::Enum { name, variants, .. } => {
            let variant_strs: Vec<String> = variants
                .iter()
                .map(|v| {
                    if let Some(payload) = &v.node.payload {
                        format!("{}({})", v.node.name, type_to_string(&payload.node))
                    } else {
                        v.node.name.clone()
                    }
                })
                .collect();
            format!("enum {} {{ {} }}", name, variant_strs.join(", "))
        }
    }
}

fn format_expr(expr: &crate::ast::Expr) -> String {
    match expr {
        crate::ast::Expr::Literal(lit) => match lit {
            crate::ast::Literal::Integer(n) => n.to_string(),
            crate::ast::Literal::Float(f) => f.to_string(),
            crate::ast::Literal::String(s) => format!("\"{}\"", s),
            crate::ast::Literal::Bool(b) => b.to_string(),
        },
        crate::ast::Expr::Ident(name) => name.clone(),
        crate::ast::Expr::Binary { op, left, right } => {
            format!(
                "({} {} {})",
                format_expr(&left.node),
                op_to_string(*op),
                format_expr(&right.node)
            )
        }
        crate::ast::Expr::Call { callee, args, .. } => {
            let args_str: Vec<String> = args.iter().map(|a| format_expr(&a.node)).collect();
            format!("{}({})", callee, args_str.join(", "))
        }
        crate::ast::Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            format!(
                "if {} {} else {}",
                format_expr(&condition.node),
                format_expr(&then_branch.node),
                format_expr(&else_branch.node)
            )
        }
        crate::ast::Expr::Block { stmts, expr } => {
            let mut s = String::from("{ ");
            for stmt in stmts {
                s.push_str(&format_stmt(&stmt.node));
                s.push(' ');
            }
            if let Some(e) = expr {
                s.push_str(&format_expr(&e.node));
                s.push(' ');
            }
            s.push('}');
            s
        }
        crate::ast::Expr::Match { expr, arms } => {
            let arms_str: Vec<String> = arms
                .iter()
                .map(|arm| {
                    format!(
                        "{} => {}",
                        format_pattern(&arm.node.pattern.node),
                        format_expr(&arm.node.body.node)
                    )
                })
                .collect();
            format!("match {} {{ {} }}", format_expr(&expr.node), arms_str.join(", "))
        }
        crate::ast::Expr::EnumVariant { variant, payload } => {
            if let Some(p) = payload {
                format!("{}({})", variant, format_expr(&p.node))
            } else {
                variant.clone()
            }
        }
    }
}

fn format_pattern(pattern: &crate::ast::Pattern) -> String {
    match pattern {
        crate::ast::Pattern::Wildcard => "_".to_string(),
        crate::ast::Pattern::Literal(lit) => match lit {
            crate::ast::Literal::Integer(n) => n.to_string(),
            crate::ast::Literal::Float(f) => f.to_string(),
            crate::ast::Literal::String(s) => format!("\"{}\"", s),
            crate::ast::Literal::Bool(b) => b.to_string(),
        },
        crate::ast::Pattern::Ident(name) => name.clone(),
        crate::ast::Pattern::Variant { name, payload } => {
            if let Some(p) = payload {
                format!("{}({})", name, format_pattern(&p.node))
            } else {
                name.clone()
            }
        }
    }
}

fn op_to_string(op: crate::ast::BinOp) -> &'static str {
    match op {
        crate::ast::BinOp::Add => "+",
        crate::ast::BinOp::Sub => "-",
        crate::ast::BinOp::Mul => "*",
        crate::ast::BinOp::Div => "/",
        crate::ast::BinOp::Mod => "%",
        crate::ast::BinOp::Eq => "==",
        crate::ast::BinOp::Ne => "!=",
        crate::ast::BinOp::Lt => "<",
        crate::ast::BinOp::Gt => ">",
        crate::ast::BinOp::Le => "<=",
        crate::ast::BinOp::Ge => ">=",
    }
}

fn type_to_string(ty: &crate::ast::Type) -> String {
    match ty {
        crate::ast::Type::Int => "Int".to_string(),
        crate::ast::Type::Float => "Float".to_string(),
        crate::ast::Type::String => "String".to_string(),
        crate::ast::Type::Bool => "Bool".to_string(),
        crate::ast::Type::Named(name) => name.clone(),
        crate::ast::Type::Generic { name, type_args } => {
            let args_str: Vec<String> = type_args.iter().map(|t| type_to_string(&t.node)).collect();
            format!("{}<{}>", name, args_str.join(", "))
        }
    }
}

fn is_incomplete_input(source: &str) -> bool {
    // Simple heuristic: check if braces are unbalanced
    let mut brace_count = 0;
    for c in source.chars() {
        match c {
            '{' => brace_count += 1,
            '}' => brace_count -= 1,
            _ => {}
        }
    }
    brace_count > 0
}

fn print_env(program: &Program) {
    if program.stmts.is_empty() {
        println!("(no definitions)");
        return;
    }

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Let { name, ty, .. } => {
                let ty_str = ty
                    .as_ref()
                    .map(|t| type_to_string(&t.node))
                    .unwrap_or_else(|| "(inferred)".to_string());
                println!("let {} : {}", name, ty_str);
            }
            Stmt::Fn {
                name,
                params,
                return_ty,
                ..
            } => {
                let params_str: Vec<String> = params
                    .iter()
                    .map(|p| format!("{}: {}", p.node.name, type_to_string(&p.node.ty.node)))
                    .collect();
                println!(
                    "fn {} : ({}) -> {}",
                    name,
                    params_str.join(", "),
                    type_to_string(&return_ty.node)
                );
            }
            Stmt::Expr(_) => {
                // Expression statements aren't persisted, but handle for completeness
            }
            Stmt::Enum { name, variants, .. } => {
                let variant_names: Vec<String> = variants
                    .iter()
                    .map(|v| {
                        if let Some(payload) = &v.node.payload {
                            format!("{}({})", v.node.name, type_to_string(&payload.node))
                        } else {
                            v.node.name.clone()
                        }
                    })
                    .collect();
                println!("enum {} {{ {} }}", name, variant_names.join(", "));
            }
        }
    }
}
