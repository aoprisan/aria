# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
cargo build              # Build the project
cargo test               # Run all tests
cargo test <name>        # Run tests matching <name>
cargo run -- file.aria   # Compile Aria source to WASM
```

## Project Overview

Aria is a compiler for a statically-typed programming language that compiles to WebAssembly. Implemented in Rust using `logos` for lexical analysis and `wasm-encoder` for code generation.

## Architecture

Pipeline: **Source → Lexer → Parser → AST → Type Checker → Typed AST → Code Generator → WASM**

### Module Structure

- **`src/lexer/`** - Tokenizer using `logos`. Handles keywords (`let`, `fn`, `if`, `else`, `true`, `false`), type keywords (`Int`, `Float`, `String`, `Bool`), operators, literals, and comments (`//`).

- **`src/parser/`** - Recursive descent parser with precedence climbing. All nodes carry span information via `Spanned<T>`.

- **`src/ast/`** - AST type definitions: `Program`, `Stmt`, `Expr`, `Type`, `Literal`, `BinOp`.

- **`src/typechecker/`** - Bidirectional type checker with two-pass approach: first registers function signatures, then type-checks bodies.
  - `types.rs` - Internal `Type` enum with `Unit` and `Function` variants
  - `typed_ast.rs` - Typed AST nodes (`TypedExpr`, `TypedStmt`, `TypedProgram`)
  - `env.rs` - Scoped symbol table for variable/function lookups

- **`src/codegen/`** - WASM code generator using `wasm-encoder`. Maps Aria types to WASM (`Int`/`Bool` → `i32`, `Float` → `f64`). All functions are exported. Top-level statements compile to a `main` function.

### Operator Precedence (highest to lowest)

1. Multiplicative (`*`, `/`, `%`) - precedence 3
2. Additive (`+`, `-`) - precedence 2
3. Comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`) - precedence 1

## Example Aria Syntax

```
let x: Int = 42;
let y = 10;                              // type annotation optional
fn add(a: Int, b: Int) -> Int { a + b }
let result = if x > 0 { x } else { 0 - x };
```
