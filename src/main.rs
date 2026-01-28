use aria::lexer::Lexer;

fn main() {
    let source = r#"
// Variable binding
let x: Int = 42;
let name: String = "aria";

// Function definition
fn add(a: Int, b: Int) -> Int {
    a + b
}

// If/else expression
let max = if x > y { x } else { y };

// Function call
let result = add(1, 2);
"#;

    println!("Lexing sample Aria code:\n{}", source);
    println!("\nTokens:");
    println!("{:-<50}", "");

    let lexer = Lexer::new(source);
    for (token, span) in lexer {
        println!("{:?} @ {:?}", token, span);
    }
}
