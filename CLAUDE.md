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

Aria is a compiler frontend for a statically-typed programming language implemented in Rust. It uses the `logos` crate for lexical analysis.

## Architecture

The compiler follows a classic pipeline: **Lexer → Parser → AST** (type checking and codegen are stubs).

### Module Structure

- **`src/lexer/`** - Tokenizer using `logos`. Handles keywords (`let`, `fn`, `if`, `else`, `true`, `false`), type keywords (`Int`, `Float`, `String`, `Bool`), operators, literals, and comments (`//`).

- **`src/parser/`** - Recursive descent parser with precedence climbing for expressions. Parses statements (let bindings, function definitions) and expressions (binary ops, function calls, if/else, blocks). All nodes carry span information.

- **`src/ast/`** - AST type definitions: `Program`, `Stmt`, `Expr`, `Type`, `Literal`, `BinOp`. Uses `Spanned<T>` wrapper for source locations.

- **`src/typechecker/`** - Placeholder module (not implemented).

- **`src/codegen/`** - Placeholder module (not implemented).

### Operator Precedence

Three-level hierarchy in parser:
1. Comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`)
2. Additive (`+`, `-`)
3. Multiplicative (`*`, `/`, `%`)

## Example Aria Syntax

```
let x: Int = 42;
fn add(a: Int, b: Int) -> Int { a + b }
let result = if x > 0 { x } else { 0 - x };
```
