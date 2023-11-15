use crate::syntax::{Span, Spanned};
use crate::types::Ty;
use ariadne::{Color, ColorGenerator, Config, Fmt, Label, Report, ReportKind, Source};
use core::fmt;

#[derive(Debug)]
pub struct TyError {
    pub at: Span,
    pub it: TyErrorData,
}

impl Spanned for TyError {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug)]
pub enum TyErrorData {
    MissingNode(String),
    InvalidLiteral,
    InvalidOperator,
    Message(String),
    UnknownVar(String),
    UnknownFunction(String),
    UnknownType(String),
    UnknownIntrinsic(String, usize),
    NonArrayIdx(Ty),
    NonStructIdx(Ty),
    ArgCountMismatch(usize, usize),
    FieldTypeMismatch {
        struct_name: String,
        field_name: String,
        expected: Ty,
        actual: Ty,
    },
    UnknownField {
        struct_name: String,
        field_name: String,
    },
    MissingField {
        struct_name: String,
        field_name: String,
    },
    TypeMismatch {
        expected: Ty,
        actual: Ty,
    },
}

impl fmt::Display for TyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.at, self.it)
    }
}

impl fmt::Display for TyErrorData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyErrorData::MissingNode(s) => write!(f, "Missing node: '{s}'"),
            TyErrorData::InvalidLiteral => write!(f, "Invalid literal"),
            TyErrorData::InvalidOperator => write!(f, "Invalid operator"),
            TyErrorData::Message(m) => write!(f, "{m}"),
            TyErrorData::UnknownVar(v) => write!(f, "Unknown variable '{v}'"),
            TyErrorData::UnknownFunction(fun) => write!(f, "Unknown function '{fun}'"),
            TyErrorData::UnknownType(t) => write!(f, "Unknown type '{t}'"),
            TyErrorData::UnknownIntrinsic(i, arg_count) =>
              write!(f, "Unknown intrinsic '{i}' with '{arg_count}' arguments"),
            TyErrorData::ArgCountMismatch(expected, actual) =>
                write!(f, "Expected {expected} arguments, but got {actual} instead"),
            TyErrorData::NonArrayIdx(t) => write!(
                f,
                "Tried to access a value of type '{t}', as if it was an array"
            ),
            TyErrorData::NonStructIdx(t) => write!(
                f,
                "Tried to access a value of type '{t}', as if it was a struct"
            ),
            TyErrorData::FieldTypeMismatch { struct_name, field_name, expected, actual } =>
              write!(f, "Expected a value of type '{expected}' for {struct_name}.{field_name}, but got '{actual}' instead"),
            TyErrorData::UnknownField { struct_name, field_name } =>
              write!(f, "Unknown field '{field_name}' for struct '{struct_name}'"),
            TyErrorData::MissingField { struct_name, field_name } =>
              write!(f, "Missing field {struct_name}.{field_name}"),
            TyErrorData::TypeMismatch { expected, actual } =>
                write!(f, "Expected type: '{expected}', but got '{actual}'"),
            }
    }
}

fn code_for_error(err_data: &TyErrorData) -> i32 {
    0
}

fn error_label(err_data: &TyErrorData) -> String {
    match err_data {
        TyErrorData::MissingNode(n) =>
            format!("Internal/Parse error: Expected a node labeled {n}"),
        TyErrorData::InvalidLiteral =>
            format!("Invalid literal couldn't be parsed"),
        TyErrorData::InvalidOperator =>
            format!("The impossible happened! An invalid operator"),
        TyErrorData::Message(m) =>
            format!("{m}"),
        TyErrorData::UnknownVar(v) =>
            format!("Unknown variable {v}"),
        TyErrorData::UnknownFunction(f) =>
            format!("Unknown function {f}"),
        TyErrorData::UnknownType(t) =>
            format!("Unknown type {t}"),
        TyErrorData::UnknownIntrinsic(f, arg_count) =>
            format!("Unknown intrinsic {f} with argcount: {arg_count}"),
        TyErrorData::NonArrayIdx(ty) =>
            format!("Tried to index into a non-array type {ty}"),
        TyErrorData::NonStructIdx(ty) =>
            format!("Tried to index into a non-struct type {ty}"),
        TyErrorData::ArgCountMismatch(expected, actual) =>
            format!("Mismatched arg count. Expected {expected} arguments, but got {actual}"),
        TyErrorData::FieldTypeMismatch { struct_name, field_name, expected, actual } =>
            format!("Mismatched field type. {struct_name}.{field_name} expects {expected}, but got {actual}"),
        TyErrorData::UnknownField { struct_name, field_name } =>
            format!("Unknown field. {struct_name} does not have a field named {field_name}"),
        TyErrorData::MissingField { struct_name, field_name } =>
            format!("Missing field. {struct_name}.{field_name} was not provided"),
        TyErrorData::TypeMismatch { expected, actual } =>
            format!("Type mismatch. Expected {expected}, but got {actual}"),
    }
}

pub fn render_ty_error(source: &str, ty_error: &TyError) -> String {
    // let mut colors = ColorGenerator::new();

    let file_name = "source.nemo";

    // Generate & choose some colours for each of our elements
    // let a = colors.next();
    // let b = colors.next();
    let out = Color::Fixed(81);
    let cache = (file_name, Source::from(source));

    let mut output: Vec<u8> = Vec::new();

    Report::build(ReportKind::Error, file_name, 12)
        .with_code(code_for_error(&ty_error.it))
        .with_message(error_label(&ty_error.it))
        .with_label(
            Label::new((
                file_name,
                ty_error.at.byte_start as usize..ty_error.at.byte_end as usize,
            ))
            .with_message(format!("{}", ty_error.it))
            .with_color(out),
        )
        // .with_note(format!(
        //     "Outputs of {} expressions must coerce to the same type",
        //     "match".fg(out)
        // ))
        .with_config(Config::default().with_color(false))
        .finish()
        .write(cache, &mut output)
        .unwrap();
    String::from_utf8(output).unwrap()
}
