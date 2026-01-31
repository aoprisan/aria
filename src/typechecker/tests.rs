use crate::parser::Parser;
use crate::typechecker::error::TypeErrorKind;
use crate::typechecker::types::Type;
use crate::typechecker::TypeChecker;

fn check(source: &str) -> Result<Type, Vec<TypeErrorKind>> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().expect("parse error");
    let mut checker = TypeChecker::new();
    checker
        .check(&program)
        .map(|typed| {
            // Return the type of the last statement's expression if any
            typed
                .stmts
                .last()
                .map(|s| match &s.kind {
                    crate::typechecker::typed_ast::TypedStmtKind::Let { ty, .. } => ty.clone(),
                    crate::typechecker::typed_ast::TypedStmtKind::Fn { return_ty, .. } => {
                        return_ty.clone()
                    }
                    crate::typechecker::typed_ast::TypedStmtKind::GenericFn { .. } => {
                        // For generic functions, return a placeholder type
                        Type::Unit
                    }
                    crate::typechecker::typed_ast::TypedStmtKind::Expr(e) => e.ty.clone(),
                    crate::typechecker::typed_ast::TypedStmtKind::Enum { name, variants } => {
                        Type::Enum {
                            name: name.clone(),
                            variants: variants.clone(),
                        }
                    }
                })
                .unwrap_or(Type::Unit)
        })
        .map_err(|errs| errs.into_iter().map(|e| e.kind).collect())
}

fn check_ok(source: &str) -> Type {
    check(source).expect("type check should succeed")
}

fn check_err(source: &str) -> TypeErrorKind {
    let errs = check(source).expect_err("type check should fail");
    errs.into_iter().next().expect("should have at least one error")
}

// ============================================================================
// Basic Type Inference
// ============================================================================

#[test]
fn infer_integer_literal() {
    assert_eq!(check_ok("let x = 42;"), Type::Int);
}

#[test]
fn infer_float_literal() {
    assert_eq!(check_ok("let x = 3.14;"), Type::Float);
}

#[test]
fn infer_string_literal() {
    assert_eq!(check_ok("let x = \"hello\";"), Type::String);
}

#[test]
fn infer_bool_literal_true() {
    assert_eq!(check_ok("let x = true;"), Type::Bool);
}

#[test]
fn infer_bool_literal_false() {
    assert_eq!(check_ok("let x = false;"), Type::Bool);
}

// ============================================================================
// Explicit Type Annotations
// ============================================================================

#[test]
fn explicit_annotation_int_ok() {
    assert_eq!(check_ok("let x: Int = 42;"), Type::Int);
}

#[test]
fn explicit_annotation_float_ok() {
    assert_eq!(check_ok("let x: Float = 3.14;"), Type::Float);
}

#[test]
fn explicit_annotation_string_ok() {
    assert_eq!(check_ok("let x: String = \"hi\";"), Type::String);
}

#[test]
fn explicit_annotation_bool_ok() {
    assert_eq!(check_ok("let x: Bool = true;"), Type::Bool);
}

#[test]
fn explicit_annotation_mismatch() {
    let err = check_err("let x: Int = \"hello\";");
    assert!(matches!(
        err,
        TypeErrorKind::TypeMismatch {
            expected: Type::Int,
            found: Type::String
        }
    ));
}

#[test]
fn explicit_annotation_mismatch_float_int() {
    let err = check_err("let x: Float = 42;");
    assert!(matches!(
        err,
        TypeErrorKind::TypeMismatch {
            expected: Type::Float,
            found: Type::Int
        }
    ));
}

// ============================================================================
// Binary Operations
// ============================================================================

#[test]
fn binary_add_int() {
    assert_eq!(check_ok("let x = 1 + 2;"), Type::Int);
}

#[test]
fn binary_add_float() {
    assert_eq!(check_ok("let x = 1.0 + 2.0;"), Type::Float);
}

#[test]
fn binary_add_string() {
    assert_eq!(check_ok("let x = \"a\" + \"b\";"), Type::String);
}

#[test]
fn binary_sub_int() {
    assert_eq!(check_ok("let x = 5 - 3;"), Type::Int);
}

#[test]
fn binary_mul_int() {
    assert_eq!(check_ok("let x = 2 * 3;"), Type::Int);
}

#[test]
fn binary_div_float() {
    assert_eq!(check_ok("let x = 6.0 / 2.0;"), Type::Float);
}

#[test]
fn binary_mod_int() {
    assert_eq!(check_ok("let x = 7 % 3;"), Type::Int);
}

#[test]
fn binary_mixed_types_error() {
    let err = check_err("let x = 1 + 1.0;");
    assert!(matches!(err, TypeErrorKind::BinaryOpTypeMismatch { .. }));
}

#[test]
fn binary_comparison_lt_int() {
    assert_eq!(check_ok("let x = 1 < 2;"), Type::Bool);
}

#[test]
fn binary_comparison_gt_float() {
    assert_eq!(check_ok("let x = 2.0 > 1.0;"), Type::Bool);
}

#[test]
fn binary_comparison_le_int() {
    assert_eq!(check_ok("let x = 1 <= 2;"), Type::Bool);
}

#[test]
fn binary_comparison_ge_int() {
    assert_eq!(check_ok("let x = 2 >= 1;"), Type::Bool);
}

#[test]
fn binary_equality_int() {
    assert_eq!(check_ok("let x = 1 == 1;"), Type::Bool);
}

#[test]
fn binary_equality_string() {
    assert_eq!(check_ok("let x = \"a\" == \"b\";"), Type::Bool);
}

#[test]
fn binary_inequality_bool() {
    assert_eq!(check_ok("let x = true != false;"), Type::Bool);
}

#[test]
fn binary_equality_mixed_error() {
    let err = check_err("let x = 1 == \"one\";");
    assert!(matches!(err, TypeErrorKind::BinaryOpTypeMismatch { .. }));
}

// ============================================================================
// Variables and Scoping
// ============================================================================

#[test]
fn variable_use() {
    assert_eq!(
        check_ok(
            r#"
            let x = 42;
            let y = x + 1;
            "#
        ),
        Type::Int
    );
}

#[test]
fn undefined_variable() {
    let err = check_err("let x = y;");
    assert!(matches!(
        err,
        TypeErrorKind::UndefinedVariable { name } if name == "y"
    ));
}

#[test]
fn variable_shadowing_in_inner_scope() {
    assert_eq!(
        check_ok(
            r#"
            let x = 42;
            let y = {
                let x = "hello";
                x
            };
            "#
        ),
        Type::String
    );
}

#[test]
fn variable_not_visible_after_block() {
    let err = check_err(
        r#"
        let y = {
            let x = 42;
            x
        };
        let z = x;
        "#,
    );
    assert!(matches!(
        err,
        TypeErrorKind::UndefinedVariable { name } if name == "x"
    ));
}

#[test]
fn duplicate_in_same_scope() {
    let err = check_err(
        r#"
        let x = 42;
        let x = 43;
        "#,
    );
    assert!(matches!(
        err,
        TypeErrorKind::DuplicateDefinition { name } if name == "x"
    ));
}

#[test]
fn shadowing_in_nested_block_allowed() {
    // This should work: inner x shadows outer x, then we can define another x after the block
    assert_eq!(
        check_ok(
            r#"
            let x = 1;
            let y = { let x = 2; x };
            let z = x + y;
            "#
        ),
        Type::Int
    );
}

// ============================================================================
// Functions
// ============================================================================

#[test]
fn function_simple() {
    assert_eq!(
        check_ok(
            r#"
            fn add(a: Int, b: Int) -> Int { a + b }
            let x = add(1, 2);
            "#
        ),
        Type::Int
    );
}

#[test]
fn function_wrong_arg_type() {
    let err = check_err(
        r#"
        fn add(a: Int, b: Int) -> Int { a + b }
        let x = add(1, "two");
        "#,
    );
    assert!(matches!(
        err,
        TypeErrorKind::TypeMismatch {
            expected: Type::Int,
            found: Type::String
        }
    ));
}

#[test]
fn function_wrong_arity() {
    let err = check_err(
        r#"
        fn add(a: Int, b: Int) -> Int { a + b }
        let x = add(1);
        "#,
    );
    assert!(matches!(
        err,
        TypeErrorKind::ArityMismatch {
            expected: 2,
            found: 1
        }
    ));
}

#[test]
fn function_undefined() {
    let err = check_err("let x = foo(1);");
    assert!(matches!(
        err,
        TypeErrorKind::UndefinedFunction { name } if name == "foo"
    ));
}

#[test]
fn function_wrong_return_type() {
    let err = check_err(
        r#"
        fn bad() -> Int { "hello" }
        "#,
    );
    assert!(matches!(
        err,
        TypeErrorKind::TypeMismatch {
            expected: Type::Int,
            found: Type::String
        }
    ));
}

#[test]
fn function_forward_reference() {
    // Functions should be usable before their definition (hoisted)
    assert_eq!(
        check_ok(
            r#"
            let x = add(1, 2);
            fn add(a: Int, b: Int) -> Int { a + b }
            "#
        ),
        Type::Int
    );
}

#[test]
fn function_no_params() {
    assert_eq!(
        check_ok(
            r#"
            fn fortytwo() -> Int { 42 }
            let x = fortytwo();
            "#
        ),
        Type::Int
    );
}

#[test]
fn call_non_function() {
    let err = check_err(
        r#"
        let x = 42;
        let y = x(1);
        "#,
    );
    assert!(matches!(err, TypeErrorKind::NotCallable { ty: Type::Int }));
}

// ============================================================================
// If Expressions
// ============================================================================

#[test]
fn if_simple() {
    assert_eq!(check_ok("let x = if true { 1 } else { 2 };"), Type::Int);
}

#[test]
fn if_condition_not_bool() {
    let err = check_err("let x = if 1 { 2 } else { 3 };");
    assert!(matches!(
        err,
        TypeErrorKind::TypeMismatch {
            expected: Type::Bool,
            found: Type::Int
        }
    ));
}

#[test]
fn if_branch_type_mismatch() {
    let err = check_err("let x = if true { 1 } else { \"two\" };");
    assert!(matches!(
        err,
        TypeErrorKind::IfBranchTypeMismatch {
            then_ty: Type::Int,
            else_ty: Type::String
        }
    ));
}

#[test]
fn if_with_comparison() {
    assert_eq!(
        check_ok(
            r#"
            let x = 5;
            let abs = if x > 0 { x } else { 0 - x };
            "#
        ),
        Type::Int
    );
}

#[test]
fn if_returns_bool() {
    assert_eq!(
        check_ok("let x = if true { 1 > 0 } else { false };"),
        Type::Bool
    );
}

// ============================================================================
// Blocks
// ============================================================================

#[test]
fn block_with_final_expr() {
    assert_eq!(
        check_ok(
            r#"
            let x = {
                let a = 1;
                let b = 2;
                a + b
            };
            "#
        ),
        Type::Int
    );
}

#[test]
fn block_without_final_expr() {
    // A block with no final expression has type Unit
    assert_eq!(
        check_ok(
            r#"
            let x = 1;
            {
                let y = 2;
            };
            "#
        ),
        Type::Unit
    );
}

// ============================================================================
// Complex Programs
// ============================================================================

#[test]
fn complex_program() {
    assert_eq!(
        check_ok(
            r#"
            let x = 42;
            let y: Int = x + 1;
            fn add(a: Int, b: Int) -> Int { a + b }
            let result = add(x, y);
            let abs = if result > 0 { result } else { 0 - result };
            "#
        ),
        Type::Int
    );
}

#[test]
fn nested_function_calls() {
    assert_eq!(
        check_ok(
            r#"
            fn double(x: Int) -> Int { x + x }
            fn quad(x: Int) -> Int { double(double(x)) }
            let x = quad(5);
            "#
        ),
        Type::Int
    );
}

// ============================================================================
// Error Span Tests
// ============================================================================

#[test]
fn error_has_correct_span() {
    let source = "let x: Int = \"hello\";";
    let mut parser = Parser::new(source);
    let program = parser.parse_program().expect("parse error");
    let mut checker = TypeChecker::new();
    let err = checker.check(&program).expect_err("should fail");
    let error = &err[0];
    // The error span should cover the string literal "hello"
    assert!(error.span.start > 0);
    assert!(error.span.end <= source.len());
}

#[test]
fn error_display_is_readable() {
    let source = "let x: Int = \"hello\";";
    let mut parser = Parser::new(source);
    let program = parser.parse_program().expect("parse error");
    let mut checker = TypeChecker::new();
    let err = checker.check(&program).expect_err("should fail");
    let msg = err[0].to_string();
    assert!(msg.contains("type error"));
    assert!(msg.contains("expected Int"));
    assert!(msg.contains("found String"));
}

// ============================================================================
// Tail Recursion Tests
// ============================================================================

#[test]
fn tailrec_simple() {
    // A simple tail-recursive function should typecheck
    assert_eq!(
        check_ok(
            r#"
            tailrec fn countdown(n: Int) -> Int {
                if n == 0 { 0 } else { countdown(n - 1) }
            }
            let x = countdown(10);
            "#
        ),
        Type::Int
    );
}

#[test]
fn tailrec_with_accumulator() {
    // Tail-recursive factorial with accumulator
    assert_eq!(
        check_ok(
            r#"
            tailrec fn factorial_acc(n: Int, acc: Int) -> Int {
                if n == 0 { acc } else { factorial_acc(n - 1, n * acc) }
            }
            let x = factorial_acc(5, 1);
            "#
        ),
        Type::Int
    );
}

#[test]
fn tailrec_in_both_branches() {
    // Recursive call in both if branches is valid
    assert_eq!(
        check_ok(
            r#"
            tailrec fn foo(n: Int) -> Int {
                if n == 0 { 0 } else { if n == 1 { foo(0) } else { foo(n - 1) } }
            }
            let x = foo(5);
            "#
        ),
        Type::Int
    );
}

#[test]
fn tailrec_not_in_tail_position_error() {
    // Recursive call not in tail position should fail
    let source = r#"
        tailrec fn bad(n: Int) -> Int {
            bad(n - 1) + 1
        }
    "#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().expect("parse error");
    let mut checker = TypeChecker::new();
    let err = checker.check(&program).expect_err("should fail");
    let msg = err[0].to_string();
    assert!(msg.contains("not in tail position"));
}

#[test]
fn tailrec_call_in_condition_error() {
    // Recursive call in condition is not in tail position
    let source = r#"
        tailrec fn bad(n: Int) -> Int {
            if bad(n) == 0 { 0 } else { 1 }
        }
    "#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().expect("parse error");
    let mut checker = TypeChecker::new();
    let err = checker.check(&program).expect_err("should fail");
    let msg = err[0].to_string();
    assert!(msg.contains("not in tail position"));
}

#[test]
fn non_tailrec_allows_non_tail_calls() {
    // Regular function allows recursive calls anywhere
    assert_eq!(
        check_ok(
            r#"
            fn factorial(n: Int) -> Int {
                if n == 0 { 1 } else { n * factorial(n - 1) }
            }
            let x = factorial(5);
            "#
        ),
        Type::Int
    );
}

#[test]
fn tailrec_mutual_recursion() {
    // Mutually recursive functions with tailrec annotation
    assert_eq!(
        check_ok(
            r#"
            tailrec fn isEven(n: Int) -> Bool {
                if n == 0 { true } else { isOdd(n - 1) }
            }
            tailrec fn isOdd(n: Int) -> Bool {
                if n == 0 { false } else { isEven(n - 1) }
            }
            let x = isEven(10);
            "#
        ),
        Type::Bool
    );
}

#[test]
fn tailrec_calls_other_function_in_tail_position() {
    // Calls to other functions in tail position are allowed
    assert_eq!(
        check_ok(
            r#"
            tailrec fn foo(n: Int) -> Int {
                if n == 0 { 0 } else { bar(n - 1) }
            }
            fn bar(n: Int) -> Int { n }
            let x = foo(5);
            "#
        ),
        Type::Int
    );
}
