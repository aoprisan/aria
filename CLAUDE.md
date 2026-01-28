# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
cargo build          # Build the project
cargo test           # Run all tests
cargo test <name>    # Run tests matching <name>
cargo run            # Run main (demo lexer)
```

## Project Overview

Aria is a compiler frontend for a statically-typed programming language implemented in Rust. Uses `logos` for lexical analysis.

## Architecture

Pipeline: **Lexer → Parser → AST → Type Checker → (codegen stub)**

### Module Structure

- **`src/lexer/`** - Tokenizer using `logos`. Handles keywords (`let`, `fn`, `if`, `else`, `true`, `false`), type keywords (`Int`, `Float`, `String`, `Bool`), operators, literals, and comments (`//`).

- **`src/parser/`** - Recursive descent parser with precedence climbing. Parses statements (let bindings, function definitions) and expressions (binary ops, function calls, if/else, blocks). All nodes carry span information via `Spanned<T>`.

- **`src/ast/`** - AST type definitions: `Program`, `Stmt`, `Expr`, `Type`, `Literal`, `BinOp`.

- **`src/typechecker/`** - Bidirectional type checker producing a typed AST. Two-pass approach: first registers function signatures, then type-checks bodies. Uses scoped environment (`Env`) for variable bindings.
  - `types.rs` - Internal `Type` enum extending AST types with `Unit` and `Function`
  - `typed_ast.rs` - Typed AST nodes (`TypedExpr`, `TypedStmt`, `TypedProgram`)
  - `env.rs` - Scoped symbol table for variable/function lookups
  - `error.rs` - Type error definitions

- **`src/codegen/`** - Placeholder module (not implemented).

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
