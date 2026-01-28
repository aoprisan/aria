use std::fs;
use std::path::Path;

use wasmtime::{Engine, Instance, Module, Store};

use crate::codegen::CodeGen;
use crate::parser::Parser;
use crate::typechecker::TypeChecker;

use super::errors::{report_codegen_error, report_parse_error, report_type_errors};

/// Check a source file for parse and type errors.
pub fn check(path: &Path) -> Result<(), i32> {
    let filename = path.display().to_string();
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read `{}`: {}", filename, e);
            return Err(1);
        }
    };

    // Parse
    let mut parser = Parser::new(&source);
    let ast = match parser.parse_program() {
        Ok(ast) => ast,
        Err(e) => {
            report_parse_error(&filename, &source, &e);
            return Err(1);
        }
    };

    // Type check
    let mut typechecker = TypeChecker::new();
    if let Err(errors) = typechecker.check(&ast) {
        report_type_errors(&filename, &source, &errors);
        return Err(1);
    }

    println!("OK: {} passed all checks", filename);
    Ok(())
}

/// Build a source file to WebAssembly.
pub fn build(path: &Path, output: Option<&Path>) -> Result<(), i32> {
    let filename = path.display().to_string();
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read `{}`: {}", filename, e);
            return Err(1);
        }
    };

    // Determine output path
    let output_path = match output {
        Some(p) => p.to_path_buf(),
        None => {
            let stem = path.file_stem().unwrap_or_default().to_string_lossy();
            Path::new(&format!("{}.wasm", stem)).to_path_buf()
        }
    };

    // Parse
    let mut parser = Parser::new(&source);
    let ast = match parser.parse_program() {
        Ok(ast) => ast,
        Err(e) => {
            report_parse_error(&filename, &source, &e);
            return Err(1);
        }
    };

    // Type check
    let mut typechecker = TypeChecker::new();
    let typed_ast = match typechecker.check(&ast) {
        Ok(typed) => typed,
        Err(errors) => {
            report_type_errors(&filename, &source, &errors);
            return Err(1);
        }
    };

    // Generate WASM
    let mut codegen = CodeGen::new();
    let wasm_bytes = match codegen.generate(&typed_ast) {
        Ok(bytes) => bytes,
        Err(e) => {
            report_codegen_error(&e);
            return Err(1);
        }
    };

    // Write output
    if let Err(e) = fs::write(&output_path, &wasm_bytes) {
        eprintln!("error: could not write `{}`: {}", output_path.display(), e);
        return Err(1);
    }

    println!(
        "Compiled {} -> {} ({} bytes)",
        filename,
        output_path.display(),
        wasm_bytes.len()
    );
    Ok(())
}

/// Compile and execute a source file.
pub fn run(path: &Path) -> Result<(), i32> {
    let filename = path.display().to_string();
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read `{}`: {}", filename, e);
            return Err(1);
        }
    };

    // Parse
    let mut parser = Parser::new(&source);
    let ast = match parser.parse_program() {
        Ok(ast) => ast,
        Err(e) => {
            report_parse_error(&filename, &source, &e);
            return Err(1);
        }
    };

    // Type check
    let mut typechecker = TypeChecker::new();
    let typed_ast = match typechecker.check(&ast) {
        Ok(typed) => typed,
        Err(errors) => {
            report_type_errors(&filename, &source, &errors);
            return Err(1);
        }
    };

    // Generate WASM
    let mut codegen = CodeGen::new();
    let wasm_bytes = match codegen.generate(&typed_ast) {
        Ok(bytes) => bytes,
        Err(e) => {
            report_codegen_error(&e);
            return Err(1);
        }
    };

    // Execute with wasmtime
    let engine = Engine::default();
    let module = match Module::new(&engine, &wasm_bytes) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: failed to load WASM module: {}", e);
            return Err(1);
        }
    };

    let mut store = Store::new(&engine, ());
    let instance = match Instance::new(&mut store, &module, &[]) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("error: failed to instantiate WASM module: {}", e);
            return Err(1);
        }
    };

    // Try to find and call main
    let main_func = match instance.get_func(&mut store, "main") {
        Some(f) => f,
        None => {
            eprintln!("error: no `main` function found in module");
            return Err(1);
        }
    };

    let main_typed = match main_func.typed::<(), i32>(&store) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("error: `main` has unexpected signature: {}", e);
            return Err(1);
        }
    };

    match main_typed.call(&mut store, ()) {
        Ok(result) => {
            println!("{}", result);
            Ok(())
        }
        Err(e) => {
            eprintln!("error: runtime error: {}", e);
            Err(1)
        }
    }
}
