use super::*;

fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source)
        .filter_map(|(result, _)| result.ok())
        .collect()
}

fn lex_with_errors(source: &str) -> Vec<Result<Token, ()>> {
    Lexer::new(source).map(|(result, _)| result).collect()
}

// =============================================================================
// Keyword Tests
// =============================================================================

#[test]
fn test_keyword_let() {
    assert_eq!(lex("let"), vec![Token::Let]);
}

#[test]
fn test_keyword_fn() {
    assert_eq!(lex("fn"), vec![Token::Fn]);
}

#[test]
fn test_keyword_if() {
    assert_eq!(lex("if"), vec![Token::If]);
}

#[test]
fn test_keyword_else() {
    assert_eq!(lex("else"), vec![Token::Else]);
}

#[test]
fn test_keyword_true() {
    assert_eq!(lex("true"), vec![Token::True]);
}

#[test]
fn test_keyword_false() {
    assert_eq!(lex("false"), vec![Token::False]);
}

#[test]
fn test_all_keywords() {
    assert_eq!(
        lex("let fn if else true false"),
        vec![
            Token::Let,
            Token::Fn,
            Token::If,
            Token::Else,
            Token::True,
            Token::False
        ]
    );
}

// =============================================================================
// Type Keyword Tests
// =============================================================================

#[test]
fn test_type_int() {
    assert_eq!(lex("Int"), vec![Token::IntType]);
}

#[test]
fn test_type_float() {
    assert_eq!(lex("Float"), vec![Token::FloatType]);
}

#[test]
fn test_type_string() {
    assert_eq!(lex("String"), vec![Token::StringType]);
}

#[test]
fn test_type_bool() {
    assert_eq!(lex("Bool"), vec![Token::BoolType]);
}

#[test]
fn test_all_type_keywords() {
    assert_eq!(
        lex("Int Float String Bool"),
        vec![
            Token::IntType,
            Token::FloatType,
            Token::StringType,
            Token::BoolType
        ]
    );
}

// =============================================================================
// Identifier Tests
// =============================================================================

#[test]
fn test_simple_identifier() {
    assert_eq!(lex("foo"), vec![Token::Ident("foo".to_string())]);
}

#[test]
fn test_identifier_with_underscore() {
    assert_eq!(
        lex("foo_bar"),
        vec![Token::Ident("foo_bar".to_string())]
    );
}

#[test]
fn test_identifier_starting_with_underscore() {
    assert_eq!(lex("_foo"), vec![Token::Ident("_foo".to_string())]);
}

#[test]
fn test_identifier_with_numbers() {
    assert_eq!(lex("foo123"), vec![Token::Ident("foo123".to_string())]);
}

#[test]
fn test_identifier_vs_keyword() {
    // Ensure keywords take precedence
    assert_eq!(lex("let"), vec![Token::Let]);
    // But similar identifiers are not keywords
    assert_eq!(lex("lets"), vec![Token::Ident("lets".to_string())]);
    assert_eq!(lex("letter"), vec![Token::Ident("letter".to_string())]);
}

#[test]
fn test_identifier_case_sensitive() {
    // Type keywords are capitalized
    assert_eq!(lex("Int"), vec![Token::IntType]);
    // Lowercase is an identifier
    assert_eq!(lex("int"), vec![Token::Ident("int".to_string())]);
}

// =============================================================================
// Integer Literal Tests
// =============================================================================

#[test]
fn test_integer_zero() {
    assert_eq!(lex("0"), vec![Token::Integer(0)]);
}

#[test]
fn test_integer_positive() {
    assert_eq!(lex("42"), vec![Token::Integer(42)]);
}

#[test]
fn test_integer_negative() {
    assert_eq!(lex("-123"), vec![Token::Integer(-123)]);
}

#[test]
fn test_integer_large() {
    assert_eq!(lex("9999999999"), vec![Token::Integer(9999999999)]);
}

// =============================================================================
// Float Literal Tests
// =============================================================================

#[test]
fn test_float_simple() {
    assert_eq!(lex("3.14"), vec![Token::Float(3.14)]);
}

#[test]
fn test_float_negative() {
    assert_eq!(lex("-0.5"), vec![Token::Float(-0.5)]);
}

#[test]
fn test_float_zero() {
    assert_eq!(lex("0.0"), vec![Token::Float(0.0)]);
}

#[test]
fn test_float_large() {
    assert_eq!(lex("123456.789"), vec![Token::Float(123456.789)]);
}

// =============================================================================
// String Literal Tests
// =============================================================================

#[test]
fn test_string_empty() {
    assert_eq!(lex(r#""""#), vec![Token::String("".to_string())]);
}

#[test]
fn test_string_simple() {
    assert_eq!(
        lex(r#""hello""#),
        vec![Token::String("hello".to_string())]
    );
}

#[test]
fn test_string_with_spaces() {
    assert_eq!(
        lex(r#""hello world""#),
        vec![Token::String("hello world".to_string())]
    );
}

#[test]
fn test_string_with_numbers() {
    assert_eq!(
        lex(r#""test123""#),
        vec![Token::String("test123".to_string())]
    );
}

// =============================================================================
// Arithmetic Operator Tests
// =============================================================================

#[test]
fn test_operator_plus() {
    assert_eq!(lex("+"), vec![Token::Plus]);
}

#[test]
fn test_operator_minus() {
    assert_eq!(lex("-"), vec![Token::Minus]);
}

#[test]
fn test_operator_star() {
    assert_eq!(lex("*"), vec![Token::Star]);
}

#[test]
fn test_operator_slash() {
    assert_eq!(lex("/"), vec![Token::Slash]);
}

#[test]
fn test_operator_percent() {
    assert_eq!(lex("%"), vec![Token::Percent]);
}

#[test]
fn test_all_arithmetic_operators() {
    assert_eq!(
        lex("+ - * / %"),
        vec![
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Percent
        ]
    );
}

// =============================================================================
// Comparison Operator Tests
// =============================================================================

#[test]
fn test_operator_eq() {
    assert_eq!(lex("=="), vec![Token::Eq]);
}

#[test]
fn test_operator_ne() {
    assert_eq!(lex("!="), vec![Token::Ne]);
}

#[test]
fn test_operator_lt() {
    assert_eq!(lex("<"), vec![Token::Lt]);
}

#[test]
fn test_operator_gt() {
    assert_eq!(lex(">"), vec![Token::Gt]);
}

#[test]
fn test_operator_le() {
    assert_eq!(lex("<="), vec![Token::Le]);
}

#[test]
fn test_operator_ge() {
    assert_eq!(lex(">="), vec![Token::Ge]);
}

#[test]
fn test_all_comparison_operators() {
    assert_eq!(
        lex("== != < > <= >="),
        vec![
            Token::Eq,
            Token::Ne,
            Token::Lt,
            Token::Gt,
            Token::Le,
            Token::Ge
        ]
    );
}

// =============================================================================
// Assignment Operator Tests
// =============================================================================

#[test]
fn test_operator_assign() {
    assert_eq!(lex("="), vec![Token::Assign]);
}

#[test]
fn test_assign_vs_eq() {
    assert_eq!(lex("= =="), vec![Token::Assign, Token::Eq]);
}

// =============================================================================
// Delimiter Tests
// =============================================================================

#[test]
fn test_delimiter_lparen() {
    assert_eq!(lex("("), vec![Token::LParen]);
}

#[test]
fn test_delimiter_rparen() {
    assert_eq!(lex(")"), vec![Token::RParen]);
}

#[test]
fn test_delimiter_lbrace() {
    assert_eq!(lex("{"), vec![Token::LBrace]);
}

#[test]
fn test_delimiter_rbrace() {
    assert_eq!(lex("}"), vec![Token::RBrace]);
}

#[test]
fn test_delimiter_comma() {
    assert_eq!(lex(","), vec![Token::Comma]);
}

#[test]
fn test_delimiter_colon() {
    assert_eq!(lex(":"), vec![Token::Colon]);
}

#[test]
fn test_delimiter_semi() {
    assert_eq!(lex(";"), vec![Token::Semi]);
}

#[test]
fn test_delimiter_arrow() {
    assert_eq!(lex("->"), vec![Token::Arrow]);
}

#[test]
fn test_all_delimiters() {
    assert_eq!(
        lex("( ) { } , : ; ->"),
        vec![
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Colon,
            Token::Semi,
            Token::Arrow
        ]
    );
}

// =============================================================================
// Whitespace Tests
// =============================================================================

#[test]
fn test_whitespace_spaces() {
    assert_eq!(lex("   let   "), vec![Token::Let]);
}

#[test]
fn test_whitespace_tabs() {
    assert_eq!(lex("\t\tlet\t\t"), vec![Token::Let]);
}

#[test]
fn test_whitespace_newlines() {
    assert_eq!(lex("\n\nlet\n\n"), vec![Token::Let]);
}

#[test]
fn test_whitespace_mixed() {
    assert_eq!(lex(" \t\n let \t\n "), vec![Token::Let]);
}

// =============================================================================
// Comment Tests
// =============================================================================

#[test]
fn test_comment_line() {
    assert_eq!(lex("// this is a comment"), vec![]);
}

#[test]
fn test_comment_before_code() {
    assert_eq!(lex("// comment\nlet"), vec![Token::Let]);
}

#[test]
fn test_comment_after_code() {
    assert_eq!(lex("let // comment"), vec![Token::Let]);
}

#[test]
fn test_comment_between_tokens() {
    assert_eq!(
        lex("let // comment\nx"),
        vec![Token::Let, Token::Ident("x".to_string())]
    );
}

// =============================================================================
// Edge Case Tests
// =============================================================================

#[test]
fn test_empty_input() {
    assert_eq!(lex(""), vec![]);
}

#[test]
fn test_whitespace_only() {
    assert_eq!(lex("   \t\n  "), vec![]);
}

#[test]
fn test_comment_only() {
    assert_eq!(lex("// just a comment"), vec![]);
}

#[test]
fn test_arrow_vs_minus_gt() {
    // Arrow should be recognized as a single token
    assert_eq!(lex("->"), vec![Token::Arrow]);
}

// =============================================================================
// Complex Expression Tests
// =============================================================================

#[test]
fn test_simple_assignment_with_expression() {
    assert_eq!(
        lex("let x = 42 + y"),
        vec![
            Token::Let,
            Token::Ident("x".to_string()),
            Token::Assign,
            Token::Integer(42),
            Token::Plus,
            Token::Ident("y".to_string())
        ]
    );
}

#[test]
fn test_variable_binding() {
    assert_eq!(
        lex("let x: Int = 42;"),
        vec![
            Token::Let,
            Token::Ident("x".to_string()),
            Token::Colon,
            Token::IntType,
            Token::Assign,
            Token::Integer(42),
            Token::Semi
        ]
    );
}

#[test]
fn test_string_binding() {
    assert_eq!(
        lex(r#"let name: String = "aria";"#),
        vec![
            Token::Let,
            Token::Ident("name".to_string()),
            Token::Colon,
            Token::StringType,
            Token::Assign,
            Token::String("aria".to_string()),
            Token::Semi
        ]
    );
}

#[test]
fn test_function_definition() {
    assert_eq!(
        lex("fn add(a: Int, b: Int) -> Int { a + b }"),
        vec![
            Token::Fn,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("a".to_string()),
            Token::Colon,
            Token::IntType,
            Token::Comma,
            Token::Ident("b".to_string()),
            Token::Colon,
            Token::IntType,
            Token::RParen,
            Token::Arrow,
            Token::IntType,
            Token::LBrace,
            Token::Ident("a".to_string()),
            Token::Plus,
            Token::Ident("b".to_string()),
            Token::RBrace
        ]
    );
}

#[test]
fn test_if_else_expression() {
    assert_eq!(
        lex("if x > y { x } else { y }"),
        vec![
            Token::If,
            Token::Ident("x".to_string()),
            Token::Gt,
            Token::Ident("y".to_string()),
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Ident("y".to_string()),
            Token::RBrace
        ]
    );
}

#[test]
fn test_function_call() {
    assert_eq!(
        lex("add(1, 2)"),
        vec![
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Integer(1),
            Token::Comma,
            Token::Integer(2),
            Token::RParen
        ]
    );
}

#[test]
fn test_arithmetic_expression() {
    assert_eq!(
        lex("(a + b) * c - d / e % f"),
        vec![
            Token::LParen,
            Token::Ident("a".to_string()),
            Token::Plus,
            Token::Ident("b".to_string()),
            Token::RParen,
            Token::Star,
            Token::Ident("c".to_string()),
            Token::Minus,
            Token::Ident("d".to_string()),
            Token::Slash,
            Token::Ident("e".to_string()),
            Token::Percent,
            Token::Ident("f".to_string())
        ]
    );
}

#[test]
fn test_comparison_expression() {
    assert_eq!(
        lex("a == b != c < d > e <= f >= g"),
        vec![
            Token::Ident("a".to_string()),
            Token::Eq,
            Token::Ident("b".to_string()),
            Token::Ne,
            Token::Ident("c".to_string()),
            Token::Lt,
            Token::Ident("d".to_string()),
            Token::Gt,
            Token::Ident("e".to_string()),
            Token::Le,
            Token::Ident("f".to_string()),
            Token::Ge,
            Token::Ident("g".to_string())
        ]
    );
}

#[test]
fn test_boolean_expression() {
    assert_eq!(
        lex("let flag: Bool = true;"),
        vec![
            Token::Let,
            Token::Ident("flag".to_string()),
            Token::Colon,
            Token::BoolType,
            Token::Assign,
            Token::True,
            Token::Semi
        ]
    );
}

#[test]
fn test_float_binding() {
    assert_eq!(
        lex("let pi: Float = 3.14;"),
        vec![
            Token::Let,
            Token::Ident("pi".to_string()),
            Token::Colon,
            Token::FloatType,
            Token::Assign,
            Token::Float(3.14),
            Token::Semi
        ]
    );
}

// =============================================================================
// Error Handling Tests
// =============================================================================

#[test]
fn test_invalid_character() {
    let results = lex_with_errors("@");
    assert_eq!(results.len(), 1);
    assert!(results[0].is_err());
}

#[test]
fn test_invalid_character_in_expression() {
    let results = lex_with_errors("let x @ 5");
    assert_eq!(results.len(), 4);
    assert_eq!(results[0], Ok(Token::Let));
    assert_eq!(results[1], Ok(Token::Ident("x".to_string())));
    assert!(results[2].is_err()); // @
    assert_eq!(results[3], Ok(Token::Integer(5)));
}

#[test]
fn test_unclosed_string() {
    let results = lex_with_errors(r#""unclosed"#);
    assert_eq!(results.len(), 1);
    assert!(results[0].is_err());
}

// =============================================================================
// Span Tests
// =============================================================================

#[test]
fn test_span_tracking() {
    let source = "let x";
    let tokens: Vec<_> = Lexer::new(source).collect();

    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0], (Ok(Token::Let), 0..3));
    assert_eq!(tokens[1], (Ok(Token::Ident("x".to_string())), 4..5));
}

#[test]
fn test_span_with_whitespace() {
    let source = "  let  ";
    let tokens: Vec<_> = Lexer::new(source).collect();

    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0], (Ok(Token::Let), 2..5));
}
