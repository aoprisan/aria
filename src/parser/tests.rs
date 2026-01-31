use super::*;
use crate::ast::{BinOp, Expr, Literal, Spanned, Stmt, Type};

fn parse(source: &str) -> ParseResult<Program> {
    Parser::new(source).parse_program()
}

fn parse_expr(source: &str) -> ParseResult<Spanned<Expr>> {
    // Wrap expression in a statement to parse it
    let source = format!("{};", source);
    let program = parse(&source)?;
    match program.stmts.into_iter().next() {
        Some(Spanned {
            node: Stmt::Expr(expr),
            ..
        }) => Ok(expr),
        _ => panic!("Expected expression statement"),
    }
}

// ============================================================================
// Literal Tests
// ============================================================================

#[test]
fn test_integer_literal() {
    let expr = parse_expr("42").unwrap();
    assert_eq!(expr.node, Expr::Literal(Literal::Integer(42)));
    assert_eq!(expr.span, 0..2);
}

#[test]
fn test_negative_integer_literal() {
    let expr = parse_expr("-123").unwrap();
    assert_eq!(expr.node, Expr::Literal(Literal::Integer(-123)));
    assert_eq!(expr.span, 0..4);
}

#[test]
fn test_float_literal() {
    let expr = parse_expr("3.14").unwrap();
    assert_eq!(expr.node, Expr::Literal(Literal::Float(3.14)));
    assert_eq!(expr.span, 0..4);
}

#[test]
fn test_string_literal() {
    let expr = parse_expr("\"hello\"").unwrap();
    assert_eq!(expr.node, Expr::Literal(Literal::String("hello".to_string())));
    assert_eq!(expr.span, 0..7);
}

#[test]
fn test_bool_true() {
    let expr = parse_expr("true").unwrap();
    assert_eq!(expr.node, Expr::Literal(Literal::Bool(true)));
    assert_eq!(expr.span, 0..4);
}

#[test]
fn test_bool_false() {
    let expr = parse_expr("false").unwrap();
    assert_eq!(expr.node, Expr::Literal(Literal::Bool(false)));
    assert_eq!(expr.span, 0..5);
}

// ============================================================================
// Identifier Tests
// ============================================================================

#[test]
fn test_identifier() {
    let expr = parse_expr("foo").unwrap();
    assert_eq!(expr.node, Expr::Ident("foo".to_string()));
    assert_eq!(expr.span, 0..3);
}

#[test]
fn test_identifier_with_underscore() {
    let expr = parse_expr("my_var").unwrap();
    assert_eq!(expr.node, Expr::Ident("my_var".to_string()));
}

// ============================================================================
// Binary Operation Tests
// ============================================================================

#[test]
fn test_simple_addition() {
    let expr = parse_expr("1 + 2").unwrap();
    match expr.node {
        Expr::Binary { op, left, right } => {
            assert_eq!(op, BinOp::Add);
            assert_eq!(left.node, Expr::Literal(Literal::Integer(1)));
            assert_eq!(right.node, Expr::Literal(Literal::Integer(2)));
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_simple_multiplication() {
    let expr = parse_expr("a * b").unwrap();
    match expr.node {
        Expr::Binary { op, left, right } => {
            assert_eq!(op, BinOp::Mul);
            assert_eq!(left.node, Expr::Ident("a".to_string()));
            assert_eq!(right.node, Expr::Ident("b".to_string()));
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_precedence_mul_over_add() {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    let expr = parse_expr("1 + 2 * 3").unwrap();
    match expr.node {
        Expr::Binary { op, left, right } => {
            assert_eq!(op, BinOp::Add);
            assert_eq!(left.node, Expr::Literal(Literal::Integer(1)));
            match right.node {
                Expr::Binary { op, left, right } => {
                    assert_eq!(op, BinOp::Mul);
                    assert_eq!(left.node, Expr::Literal(Literal::Integer(2)));
                    assert_eq!(right.node, Expr::Literal(Literal::Integer(3)));
                }
                _ => panic!("Expected binary expression on right"),
            }
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_left_associativity() {
    // 1 - 2 - 3 should parse as (1 - 2) - 3
    let expr = parse_expr("1 - 2 - 3").unwrap();
    match expr.node {
        Expr::Binary { op, left, right } => {
            assert_eq!(op, BinOp::Sub);
            assert_eq!(right.node, Expr::Literal(Literal::Integer(3)));
            match left.node {
                Expr::Binary { op, left, right } => {
                    assert_eq!(op, BinOp::Sub);
                    assert_eq!(left.node, Expr::Literal(Literal::Integer(1)));
                    assert_eq!(right.node, Expr::Literal(Literal::Integer(2)));
                }
                _ => panic!("Expected binary expression on left"),
            }
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_comparison_lowest_precedence() {
    // a + b == c * d should parse as (a + b) == (c * d)
    let expr = parse_expr("a + b == c * d").unwrap();
    match expr.node {
        Expr::Binary { op, left, right } => {
            assert_eq!(op, BinOp::Eq);
            match left.node {
                Expr::Binary { op, .. } => assert_eq!(op, BinOp::Add),
                _ => panic!("Expected binary expression on left"),
            }
            match right.node {
                Expr::Binary { op, .. } => assert_eq!(op, BinOp::Mul),
                _ => panic!("Expected binary expression on right"),
            }
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_all_comparison_operators() {
    let ops = [
        ("a == b", BinOp::Eq),
        ("a != b", BinOp::Ne),
        ("a < b", BinOp::Lt),
        ("a > b", BinOp::Gt),
        ("a <= b", BinOp::Le),
        ("a >= b", BinOp::Ge),
    ];

    for (source, expected_op) in ops {
        let expr = parse_expr(source).unwrap();
        match expr.node {
            Expr::Binary { op, .. } => assert_eq!(op, expected_op, "Failed for {}", source),
            _ => panic!("Expected binary expression for {}", source),
        }
    }
}

#[test]
fn test_all_arithmetic_operators() {
    let ops = [
        ("a + b", BinOp::Add),
        ("a - b", BinOp::Sub),
        ("a * b", BinOp::Mul),
        ("a / b", BinOp::Div),
        ("a % b", BinOp::Mod),
    ];

    for (source, expected_op) in ops {
        let expr = parse_expr(source).unwrap();
        match expr.node {
            Expr::Binary { op, .. } => assert_eq!(op, expected_op, "Failed for {}", source),
            _ => panic!("Expected binary expression for {}", source),
        }
    }
}

// ============================================================================
// Grouped Expression Tests
// ============================================================================

#[test]
fn test_grouped_expression() {
    // (1 + 2) * 3 should respect parentheses
    let expr = parse_expr("(1 + 2) * 3").unwrap();
    match expr.node {
        Expr::Binary { op, left, right } => {
            assert_eq!(op, BinOp::Mul);
            match left.node {
                Expr::Binary { op, .. } => assert_eq!(op, BinOp::Add),
                _ => panic!("Expected binary expression in parens"),
            }
            assert_eq!(right.node, Expr::Literal(Literal::Integer(3)));
        }
        _ => panic!("Expected binary expression"),
    }
}

// ============================================================================
// Function Call Tests
// ============================================================================

#[test]
fn test_function_call_no_args() {
    let expr = parse_expr("foo()").unwrap();
    match expr.node {
        Expr::Call { callee, args } => {
            assert_eq!(callee, "foo");
            assert!(args.is_empty());
        }
        _ => panic!("Expected call expression"),
    }
}

#[test]
fn test_function_call_single_arg() {
    let expr = parse_expr("foo(42)").unwrap();
    match expr.node {
        Expr::Call { callee, args } => {
            assert_eq!(callee, "foo");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0].node, Expr::Literal(Literal::Integer(42)));
        }
        _ => panic!("Expected call expression"),
    }
}

#[test]
fn test_function_call_multiple_args() {
    let expr = parse_expr("add(1, 2, 3)").unwrap();
    match expr.node {
        Expr::Call { callee, args } => {
            assert_eq!(callee, "add");
            assert_eq!(args.len(), 3);
            assert_eq!(args[0].node, Expr::Literal(Literal::Integer(1)));
            assert_eq!(args[1].node, Expr::Literal(Literal::Integer(2)));
            assert_eq!(args[2].node, Expr::Literal(Literal::Integer(3)));
        }
        _ => panic!("Expected call expression"),
    }
}

#[test]
fn test_function_call_expression_args() {
    let expr = parse_expr("add(1 + 2, x * y)").unwrap();
    match expr.node {
        Expr::Call { callee, args } => {
            assert_eq!(callee, "add");
            assert_eq!(args.len(), 2);
            match &args[0].node {
                Expr::Binary { op, .. } => assert_eq!(*op, BinOp::Add),
                _ => panic!("Expected binary expression as first arg"),
            }
            match &args[1].node {
                Expr::Binary { op, .. } => assert_eq!(*op, BinOp::Mul),
                _ => panic!("Expected binary expression as second arg"),
            }
        }
        _ => panic!("Expected call expression"),
    }
}

// ============================================================================
// If/Else Tests
// ============================================================================

#[test]
fn test_simple_if_else() {
    let expr = parse_expr("if true { 1 } else { 2 }").unwrap();
    match expr.node {
        Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert_eq!(condition.node, Expr::Literal(Literal::Bool(true)));
            match then_branch.node {
                Expr::Block { stmts, expr } => {
                    assert!(stmts.is_empty());
                    assert_eq!(
                        expr.as_ref().unwrap().node,
                        Expr::Literal(Literal::Integer(1))
                    );
                }
                _ => panic!("Expected block in then branch"),
            }
            match else_branch.node {
                Expr::Block { stmts, expr } => {
                    assert!(stmts.is_empty());
                    assert_eq!(
                        expr.as_ref().unwrap().node,
                        Expr::Literal(Literal::Integer(2))
                    );
                }
                _ => panic!("Expected block in else branch"),
            }
        }
        _ => panic!("Expected if expression"),
    }
}

#[test]
fn test_if_with_comparison_condition() {
    let expr = parse_expr("if x > y { x } else { y }").unwrap();
    match expr.node {
        Expr::If { condition, .. } => match condition.node {
            Expr::Binary { op, .. } => assert_eq!(op, BinOp::Gt),
            _ => panic!("Expected binary comparison"),
        },
        _ => panic!("Expected if expression"),
    }
}

// ============================================================================
// Block Tests
// ============================================================================

#[test]
fn test_block_single_expr() {
    let expr = parse_expr("{ 42 }").unwrap();
    match expr.node {
        Expr::Block { stmts, expr } => {
            assert!(stmts.is_empty());
            assert_eq!(
                expr.as_ref().unwrap().node,
                Expr::Literal(Literal::Integer(42))
            );
        }
        _ => panic!("Expected block expression"),
    }
}

#[test]
fn test_block_with_statements() {
    let expr = parse_expr("{ let x: Int = 1; x }").unwrap();
    match expr.node {
        Expr::Block { stmts, expr } => {
            assert_eq!(stmts.len(), 1);
            match &stmts[0].node {
                Stmt::Let { name, .. } => assert_eq!(name, "x"),
                _ => panic!("Expected let statement"),
            }
            assert_eq!(expr.as_ref().unwrap().node, Expr::Ident("x".to_string()));
        }
        _ => panic!("Expected block expression"),
    }
}

#[test]
fn test_block_without_final_expr() {
    let expr = parse_expr("{ let x: Int = 1; }").unwrap();
    match expr.node {
        Expr::Block { stmts, expr } => {
            assert_eq!(stmts.len(), 1);
            assert!(expr.is_none());
        }
        _ => panic!("Expected block expression"),
    }
}

// ============================================================================
// Let Statement Tests
// ============================================================================

#[test]
fn test_let_simple() {
    let program = parse("let x: Int = 42;").unwrap();
    assert_eq!(program.stmts.len(), 1);
    match &program.stmts[0].node {
        Stmt::Let { name, ty, value } => {
            assert_eq!(name, "x");
            assert_eq!(ty.as_ref().unwrap().node, Type::Int);
            assert_eq!(value.node, Expr::Literal(Literal::Integer(42)));
        }
        _ => panic!("Expected let statement"),
    }
}

#[test]
fn test_let_with_expression() {
    let program = parse("let result: Int = 1 + 2 * 3;").unwrap();
    assert_eq!(program.stmts.len(), 1);
    match &program.stmts[0].node {
        Stmt::Let { name, ty, value } => {
            assert_eq!(name, "result");
            assert_eq!(ty.as_ref().unwrap().node, Type::Int);
            match &value.node {
                Expr::Binary { op, .. } => assert_eq!(*op, BinOp::Add),
                _ => panic!("Expected binary expression"),
            }
        }
        _ => panic!("Expected let statement"),
    }
}

#[test]
fn test_let_all_types() {
    let types = [
        ("let x: Int = 1;", Type::Int),
        ("let x: Float = 1.0;", Type::Float),
        ("let x: String = \"hi\";", Type::String),
        ("let x: Bool = true;", Type::Bool),
    ];

    for (source, expected_ty) in types {
        let program = parse(source).unwrap();
        match &program.stmts[0].node {
            Stmt::Let { ty, .. } => {
                assert_eq!(ty.as_ref().unwrap().node, expected_ty, "Failed for {}", source)
            }
            _ => panic!("Expected let statement for {}", source),
        }
    }
}

// ============================================================================
// Function Definition Tests
// ============================================================================

#[test]
fn test_fn_no_params() {
    let program = parse("fn foo() -> Int { 42 }").unwrap();
    assert_eq!(program.stmts.len(), 1);
    match &program.stmts[0].node {
        Stmt::Fn {
            name,
            params,
            return_ty,
            body,
            ..
        } => {
            assert_eq!(name, "foo");
            assert!(params.is_empty());
            assert_eq!(return_ty.node, Type::Int);
            match &body.node {
                Expr::Block { expr, .. } => {
                    assert_eq!(
                        expr.as_ref().unwrap().node,
                        Expr::Literal(Literal::Integer(42))
                    );
                }
                _ => panic!("Expected block body"),
            }
        }
        _ => panic!("Expected fn statement"),
    }
}

#[test]
fn test_fn_single_param() {
    let program = parse("fn square(x: Int) -> Int { x * x }").unwrap();
    match &program.stmts[0].node {
        Stmt::Fn { name, params, .. } => {
            assert_eq!(name, "square");
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].node.name, "x");
            assert_eq!(params[0].node.ty.node, Type::Int);
        }
        _ => panic!("Expected fn statement"),
    }
}

#[test]
fn test_fn_multiple_params() {
    let program = parse("fn add(a: Int, b: Int) -> Int { a + b }").unwrap();
    match &program.stmts[0].node {
        Stmt::Fn { name, params, .. } => {
            assert_eq!(name, "add");
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].node.name, "a");
            assert_eq!(params[0].node.ty.node, Type::Int);
            assert_eq!(params[1].node.name, "b");
            assert_eq!(params[1].node.ty.node, Type::Int);
        }
        _ => panic!("Expected fn statement"),
    }
}

// ============================================================================
// Tailrec Function Tests
// ============================================================================

#[test]
fn test_tailrec_fn() {
    let program = parse("tailrec fn countdown(n: Int) -> Int { n }").unwrap();
    match &program.stmts[0].node {
        Stmt::Fn { name, is_tailrec, .. } => {
            assert_eq!(name, "countdown");
            assert!(*is_tailrec);
        }
        _ => panic!("Expected fn statement"),
    }
}

#[test]
fn test_regular_fn_not_tailrec() {
    let program = parse("fn add(a: Int, b: Int) -> Int { a + b }").unwrap();
    match &program.stmts[0].node {
        Stmt::Fn { name, is_tailrec, .. } => {
            assert_eq!(name, "add");
            assert!(!*is_tailrec);
        }
        _ => panic!("Expected fn statement"),
    }
}

// ============================================================================
// Span Tracking Tests
// ============================================================================

#[test]
fn test_span_integer() {
    let expr = parse_expr("42").unwrap();
    assert_eq!(expr.span, 0..2);
}

#[test]
fn test_span_binary_expr() {
    let expr = parse_expr("1 + 2").unwrap();
    assert_eq!(expr.span, 0..5);
    match expr.node {
        Expr::Binary { left, right, .. } => {
            assert_eq!(left.span, 0..1);
            assert_eq!(right.span, 4..5);
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_span_nested_binary() {
    let expr = parse_expr("1 + 2 * 3").unwrap();
    assert_eq!(expr.span, 0..9);
}

#[test]
fn test_span_function_call() {
    let expr = parse_expr("foo(1, 2)").unwrap();
    assert_eq!(expr.span, 0..9);
}

#[test]
fn test_span_let_stmt() {
    let program = parse("let x: Int = 42;").unwrap();
    assert_eq!(program.stmts[0].span, 0..16);
}

#[test]
fn test_span_fn_stmt() {
    let program = parse("fn foo() -> Int { 42 }").unwrap();
    assert_eq!(program.stmts[0].span, 0..22);
}

// ============================================================================
// Error Case Tests
// ============================================================================

#[test]
fn test_error_missing_semicolon() {
    let result = parse("let x: Int = 42");
    assert!(result.is_err());
    match result.unwrap_err().kind {
        ParseErrorKind::UnexpectedEof { expected } => {
            assert!(expected.iter().any(|e| e.contains("Semi")));
        }
        _ => panic!("Expected UnexpectedEof error"),
    }
}

#[test]
fn test_let_without_type_annotation() {
    // Type annotation is now optional
    let program = parse("let x = 42;").unwrap();
    assert_eq!(program.stmts.len(), 1);
    match &program.stmts[0].node {
        Stmt::Let { name, ty, value } => {
            assert_eq!(name, "x");
            assert!(ty.is_none());
            assert_eq!(value.node, Expr::Literal(Literal::Integer(42)));
        }
        _ => panic!("Expected let statement"),
    }
}

#[test]
fn test_error_unclosed_paren() {
    let result = parse("foo(1, 2;");
    assert!(result.is_err());
}

#[test]
fn test_error_missing_else_branch() {
    let result = parse("if true { 1 };");
    assert!(result.is_err());
}

#[test]
fn test_error_missing_function_body() {
    let result = parse("fn foo() -> Int;");
    assert!(result.is_err());
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_program_multiple_statements() {
    let source = r#"
        fn add(a: Int, b: Int) -> Int { a + b }
        let result: Int = add(1, 2);
    "#;
    let program = parse(source).unwrap();
    assert_eq!(program.stmts.len(), 2);
    match &program.stmts[0].node {
        Stmt::Fn { name, .. } => assert_eq!(name, "add"),
        _ => panic!("Expected fn statement"),
    }
    match &program.stmts[1].node {
        Stmt::Let { name, .. } => assert_eq!(name, "result"),
        _ => panic!("Expected let statement"),
    }
}

#[test]
fn test_complex_expression() {
    let expr = parse_expr("if x > 0 { x * 2 } else { x + 1 }").unwrap();
    match expr.node {
        Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            // Condition is x > 0
            match condition.node {
                Expr::Binary { op, .. } => assert_eq!(op, BinOp::Gt),
                _ => panic!("Expected comparison"),
            }
            // Then branch is x * 2
            match &then_branch.node {
                Expr::Block { expr: Some(e), .. } => match &e.node {
                    Expr::Binary { op, .. } => assert_eq!(*op, BinOp::Mul),
                    _ => panic!("Expected multiplication"),
                },
                _ => panic!("Expected block with expression"),
            }
            // Else branch is x + 1
            match &else_branch.node {
                Expr::Block { expr: Some(e), .. } => match &e.node {
                    Expr::Binary { op, .. } => assert_eq!(*op, BinOp::Add),
                    _ => panic!("Expected addition"),
                },
                _ => panic!("Expected block with expression"),
            }
        }
        _ => panic!("Expected if expression"),
    }
}

#[test]
fn test_nested_function_calls() {
    let expr = parse_expr("foo(bar(1), baz(2, 3))").unwrap();
    match expr.node {
        Expr::Call { callee, args } => {
            assert_eq!(callee, "foo");
            assert_eq!(args.len(), 2);
            match &args[0].node {
                Expr::Call { callee, args } => {
                    assert_eq!(callee, "bar");
                    assert_eq!(args.len(), 1);
                }
                _ => panic!("Expected call expression"),
            }
            match &args[1].node {
                Expr::Call { callee, args } => {
                    assert_eq!(callee, "baz");
                    assert_eq!(args.len(), 2);
                }
                _ => panic!("Expected call expression"),
            }
        }
        _ => panic!("Expected call expression"),
    }
}
