use std::env;
use std::fs;
use std::path::Path;
use std::process;

use aria::codegen::CodeGen;
use aria::parser::Parser;
use aria::typechecker::TypeChecker;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <input.aria> [output.wasm]", args[0]);
        eprintln!("       Compiles an Aria source file to WebAssembly.");
        process::exit(1);
    }

    let input_path = &args[1];
    let output_path = if args.len() > 2 {
        args[2].clone()
    } else {
        // Default output: replace .aria with .wasm, or append .wasm
        let path = Path::new(input_path);
        let stem = path.file_stem().unwrap_or_default().to_string_lossy();
        format!("{}.wasm", stem)
    };

    // Read source file
    let source = match fs::read_to_string(input_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading '{}': {}", input_path, e);
            process::exit(1);
        }
    };

    // Parse
    let mut parser = Parser::new(&source);
    let ast = match parser.parse_program() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            process::exit(1);
        }
    };

    // Type check
    let mut typechecker = TypeChecker::new();
    let typed_ast = match typechecker.check(&ast) {
        Ok(typed) => typed,
        Err(errors) => {
            for err in errors {
                eprintln!("Type error: {}", err);
            }
            process::exit(1);
        }
    };

    // Generate WASM
    let mut codegen = CodeGen::new();
    let wasm_bytes = match codegen.generate(&typed_ast) {
        Ok(bytes) => bytes,
        Err(e) => {
            eprintln!("Code generation error: {}", e);
            process::exit(1);
        }
    };

    // Write output
    if let Err(e) = fs::write(&output_path, &wasm_bytes) {
        eprintln!("Error writing '{}': {}", output_path, e);
        process::exit(1);
    }

    println!("Compiled {} -> {} ({} bytes)", input_path, output_path, wasm_bytes.len());
}
