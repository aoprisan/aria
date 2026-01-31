use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::codegen::error::CodeGenError;
use crate::parser::{ParseError, ParseErrorKind};
use crate::typechecker::error::{TypeError, TypeErrorKind};

/// Report a parse error with source context using ariadne.
pub fn report_parse_error(filename: &str, source: &str, error: &ParseError) {
    let message = match &error.kind {
        ParseErrorKind::UnexpectedToken { expected, found } => {
            format!(
                "unexpected token: expected {}, found {}",
                format_expected(expected),
                found
                    .as_ref()
                    .map(|t| format!("{:?}", t))
                    .unwrap_or_else(|| "nothing".to_string())
            )
        }
        ParseErrorKind::UnexpectedEof { expected } => {
            format!(
                "unexpected end of file: expected {}",
                format_expected(expected)
            )
        }
        ParseErrorKind::LexerError => "invalid token".to_string(),
    };

    Report::build(ReportKind::Error, filename, error.span.start)
        .with_message("parse error")
        .with_label(
            Label::new((filename, error.span.clone()))
                .with_message(message)
                .with_color(Color::Red),
        )
        .finish()
        .eprint((filename, Source::from(source)))
        .expect("failed to print error report");
}

/// Report type errors with source context using ariadne.
pub fn report_type_errors(filename: &str, source: &str, errors: &[TypeError]) {
    for error in errors {
        report_type_error(filename, source, error);
    }
}

fn report_type_error(filename: &str, source: &str, error: &TypeError) {
    let message = match &error.kind {
        TypeErrorKind::TypeMismatch { expected, found } => {
            format!("expected `{}`, found `{}`", expected, found)
        }
        TypeErrorKind::UndefinedVariable { name } => {
            format!("cannot find variable `{}` in this scope", name)
        }
        TypeErrorKind::UndefinedFunction { name } => {
            format!("cannot find function `{}` in this scope", name)
        }
        TypeErrorKind::DuplicateDefinition { name } => {
            format!("`{}` is already defined in this scope", name)
        }
        TypeErrorKind::NotCallable { ty } => {
            format!("expected function, found `{}`", ty)
        }
        TypeErrorKind::ArityMismatch { expected, found } => {
            format!(
                "function expects {} argument{}, but {} {} supplied",
                expected,
                if *expected == 1 { "" } else { "s" },
                found,
                if *found == 1 { "was" } else { "were" }
            )
        }
        TypeErrorKind::BinaryOpTypeMismatch { op, left, right } => {
            format!(
                "cannot apply `{:?}` to `{}` and `{}`",
                op, left, right
            )
        }
        TypeErrorKind::IfBranchTypeMismatch { then_ty, else_ty } => {
            format!(
                "`if` and `else` have incompatible types: expected `{}`, found `{}`",
                then_ty, else_ty
            )
        }
        TypeErrorKind::NotTailRecursive { func_name } => {
            format!(
                "recursive call to `{}` is not in tail position",
                func_name
            )
        }
    };

    let title = match &error.kind {
        TypeErrorKind::TypeMismatch { .. } => "mismatched types",
        TypeErrorKind::UndefinedVariable { .. } => "undefined variable",
        TypeErrorKind::UndefinedFunction { .. } => "undefined function",
        TypeErrorKind::DuplicateDefinition { .. } => "duplicate definition",
        TypeErrorKind::NotCallable { .. } => "not callable",
        TypeErrorKind::ArityMismatch { .. } => "wrong number of arguments",
        TypeErrorKind::BinaryOpTypeMismatch { .. } => "invalid operator",
        TypeErrorKind::IfBranchTypeMismatch { .. } => "incompatible branch types",
        TypeErrorKind::NotTailRecursive { .. } => "not tail recursive",
    };

    Report::build(ReportKind::Error, filename, error.span.start)
        .with_message(title)
        .with_label(
            Label::new((filename, error.span.clone()))
                .with_message(message)
                .with_color(Color::Red),
        )
        .finish()
        .eprint((filename, Source::from(source)))
        .expect("failed to print error report");
}

/// Report a code generation error (no span info available).
pub fn report_codegen_error(error: &CodeGenError) {
    eprintln!("error: {}", error);
}

fn format_expected(expected: &[String]) -> String {
    match expected.len() {
        0 => "nothing".to_string(),
        1 => expected[0].clone(),
        2 => format!("{} or {}", expected[0], expected[1]),
        _ => {
            let (last, rest) = expected.split_last().unwrap();
            format!("{}, or {}", rest.join(", "), last)
        }
    }
}
