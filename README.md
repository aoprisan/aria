# Aria

A statically-typed programming language that compiles to WebAssembly, implemented in Rust.

## Features

- **Static typing** with optional type inference for local variables
- **First-class functions** with explicit parameter and return types
- **Expression-oriented** design - if/else and blocks return values
- **WebAssembly output** - compile directly to `.wasm` files

## Installation

```bash
cargo build --release
```

## Usage

```bash
# Compile an Aria source file to WebAssembly
aria input.aria              # outputs input.wasm
aria input.aria output.wasm  # specify output path
```

## Language Overview

### Variables

```
let x: Int = 42;
let y = 10;          // type inferred as Int
let pi: Float = 3.14;
let flag: Bool = true;
```

### Functions

```
fn add(a: Int, b: Int) -> Int {
    a + b
}

fn max(x: Int, y: Int) -> Int {
    if x > y { x } else { y }
}
```

### Expressions

```
// Arithmetic: +, -, *, /, %
let sum = 1 + 2 * 3;

// Comparisons: ==, !=, <, >, <=, >=
let is_positive = x > 0;

// If-else (returns a value)
let abs = if x >= 0 { x } else { 0 - x };

// Blocks (returns last expression)
let result = {
    let temp = x * 2;
    temp + 1
};
```

### Types

| Type    | Description              |
|---------|--------------------------|
| `Int`   | 64-bit signed integer    |
| `Float` | 64-bit floating point    |
| `Bool`  | Boolean (`true`/`false`) |
| `String`| String (parsing only)    |

### Comments

```
// Single-line comments
let x = 42;  // inline comment
```

## Example

```
// factorial.aria
fn factorial(n: Int) -> Int {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

let result = factorial(5);  // 120
```

Compile and run:

```bash
aria factorial.aria
# Run with any WASM runtime (wasmtime, wasmer, etc.)
wasmtime factorial.wasm --invoke main
```

## Architecture

```
Source Code → Lexer → Parser → AST → Type Checker → Code Generator → WASM
```

| Stage | Description |
|-------|-------------|
| Lexer | Tokenizes source using [logos](https://github.com/maciejhirsz/logos) |
| Parser | Recursive descent with precedence climbing |
| Type Checker | Bidirectional type checking with inference |
| Code Generator | Emits WebAssembly using [wasm-encoder](https://github.com/bytecodealliance/wasm-tools) |

## Development

```bash
cargo build          # Build
cargo test           # Run tests
cargo run -- file.aria   # Compile a file
```

## Operator Precedence

From highest to lowest:

1. `*`, `/`, `%` (multiplicative)
2. `+`, `-` (additive)
3. `==`, `!=`, `<`, `>`, `<=`, `>=` (comparison)

## License

MIT
