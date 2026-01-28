# Aria Language Support for VS Code

Syntax highlighting for the [Aria](https://github.com/aria-lang/aria) programming language.

## Features

- Syntax highlighting for `.aria` files
- Bracket matching and auto-closing
- Comment toggling with `Ctrl+/` (or `Cmd+/` on macOS)

## Highlighted Elements

- **Keywords**: `let`, `fn`, `if`, `else`
- **Types**: `Int`, `Float`, `String`, `Bool`
- **Literals**: strings, integers, floats, booleans (`true`, `false`)
- **Comments**: line comments (`// ...`)
- **Operators**: arithmetic, comparison, assignment

## Installation

### From Source (Development)

1. Copy or symlink this folder to your VS Code extensions directory:
   - **macOS/Linux**: `~/.vscode/extensions/aria-language`
   - **Windows**: `%USERPROFILE%\.vscode\extensions\aria-language`

2. Reload VS Code

### Using VS Code

1. Open this folder in VS Code
2. Press `F5` to launch an Extension Development Host
3. Open any `.aria` file to see syntax highlighting

## Example

```aria
// Calculate factorial
fn factorial(n: Int) -> Int {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

let result: Int = factorial(5);
let message = "Hello, Aria!";
let pi: Float = 3.14159;
let enabled: Bool = true;
```

## License

MIT
