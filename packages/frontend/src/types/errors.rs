use crate::types::Ty;
use ariadne::{Color, Config, Label, Report, ReportKind, Source};
use backend::ir::{Name, NameMap};
use core::fmt;
use line_index::{LineCol, LineIndex};
use rowan::TextRange;
use std::str;

#[derive(Debug)]
pub struct TyError {
    pub at: TextRange,
    pub it: TyErrorData,
}

impl TyError {
    pub fn display<'err, 'src>(
        &'err self,
        source: &'src str,
        name_map: &'src NameMap,
    ) -> TyErrorDisplay<'src, 'err> {
        TyErrorDisplay {
            source,
            name_map,
            ty_error: self,
        }
    }

    pub fn line_col(&self, line_index: &LineIndex) -> (LineCol, LineCol) {
        let start = line_index.line_col(self.at.start());
        let end = line_index.line_col(self.at.end());
        (start, end)
    }

    pub fn message(&self, name_map: &NameMap) -> String {
        error_label(&self.it, name_map)
    }
}

pub struct TyErrorDisplay<'src, 'err> {
    ty_error: &'err TyError,
    source: &'src str,
    name_map: &'src NameMap,
}

impl fmt::Display for TyErrorDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        render_ty_error(self.source, self.name_map, self.ty_error, true, f);
        Ok(())
    }
}

#[derive(Debug)]
pub enum TyErrorData {
    MissingNode(String),
    InvalidLiteral,
    InvalidOperator,
    CantInferEmptyArray,
    Message(String),
    UnknownVar(String),
    UnknownFunction(String),
    UnknownType(String),
    UnknownIntrinsic(String, usize),
    NonArrayIdx(Ty),
    NonStructIdx(Ty),
    NotAFunction(Ty),
    NonFunctionImport {
        name: Name,
        ty: Ty,
    },
    ArgCountMismatch(usize, usize),
    FieldTypeMismatch {
        struct_name: Name,
        field_name: Name,
        expected: Ty,
        actual: Ty,
    },
    UnknownField {
        struct_name: Name,
        field_name: String,
    },
    MissingField {
        struct_name: Name,
        field_name: Name,
    },
    TypeMismatch {
        expected: Ty,
        actual: Ty,
    },
}

fn code_for_error(err_data: &TyErrorData) -> i32 {
    match err_data {
        TyErrorData::MissingNode(_) => 1,
        TyErrorData::InvalidLiteral => 2,
        TyErrorData::InvalidOperator => 3,
        TyErrorData::Message(_) => 4,
        TyErrorData::UnknownVar(_) => 5,
        TyErrorData::UnknownFunction(_) => 6,
        TyErrorData::UnknownType(_) => 7,
        TyErrorData::UnknownIntrinsic(_, _) => 8,
        TyErrorData::NonArrayIdx(_) => 9,
        TyErrorData::NonStructIdx(_) => 10,
        TyErrorData::ArgCountMismatch(_, _) => 11,
        TyErrorData::FieldTypeMismatch { .. } => 12,
        TyErrorData::UnknownField { .. } => 13,
        TyErrorData::MissingField { .. } => 14,
        TyErrorData::TypeMismatch { .. } => 15,
        TyErrorData::NotAFunction(_) => 16,
        TyErrorData::CantInferEmptyArray => 17,
        TyErrorData::NonFunctionImport { .. } => 18,
    }
}

fn error_label(err_data: &TyErrorData, name_map: &NameMap) -> String {
    match err_data {
        TyErrorData::MissingNode(n) => format!("Internal/Parse error: Expected a node labeled {n}"),
        TyErrorData::InvalidLiteral => "Invalid literal couldn't be parsed".to_string(),
        TyErrorData::InvalidOperator => "The impossible happened! An invalid operator".to_string(),
        TyErrorData::CantInferEmptyArray => "Can't infer type of an empty array".to_string(),
        TyErrorData::Message(m) => m.clone(),
        TyErrorData::UnknownVar(v) => format!("Unknown variable {v}"),
        TyErrorData::UnknownFunction(f) => format!("Unknown function {f}"),
        TyErrorData::UnknownType(t) => format!("Unknown type {t}"),
        TyErrorData::UnknownIntrinsic(f, arg_count) => {
            format!("Unknown intrinsic {f} with argcount: {arg_count}")
        }
        TyErrorData::NonArrayIdx(ty) => format!(
            "Tried to index into a non-array type {}",
            ty.display(name_map)
        ),
        TyErrorData::NonStructIdx(ty) => format!(
            "Tried to index into a non-struct type {}",
            ty.display(name_map)
        ),
        TyErrorData::NotAFunction(ty) => format!(
            "Can't a call a value of type {} as a function",
            ty.display(name_map)
        ),
        TyErrorData::NonFunctionImport { name, ty } => format!(
            "Can't import a non-function value. {} is of type {}",
            name_map.get(name).unwrap().it,
            ty.display(name_map)
        ),
        TyErrorData::ArgCountMismatch(expected, actual) => format!(
            "Mismatched arg count. Expected {expected} {}, but got {actual}",
            if *expected == 1 {
                "argument"
            } else {
                "arguments"
            }
        ),
        TyErrorData::FieldTypeMismatch {
            struct_name,
            field_name,
            expected,
            actual,
        } => format!(
            "Mismatched field type. {}.{} expects {}, but got {}",
            name_map.get(struct_name).unwrap().it,
            name_map.get(field_name).unwrap().it,
            expected.display(name_map),
            actual.display(name_map)
        ),
        TyErrorData::UnknownField {
            struct_name,
            field_name,
        } => format!(
            "Unknown field. {} does not have a field named {field_name}",
            name_map.get(struct_name).unwrap().it
        ),
        TyErrorData::MissingField {
            struct_name,
            field_name,
        } => format!(
            "Missing field. {}.{} was not provided",
            name_map.get(struct_name).unwrap().it,
            name_map.get(field_name).unwrap().it
        ),
        TyErrorData::TypeMismatch { expected, actual } => format!(
            "Type mismatch. Expected {}, but got {}",
            expected.display(name_map),
            actual.display(name_map)
        ),
    }
}

pub fn render_ty_error(
    source: &str,
    name_map: &NameMap,
    ty_error: &TyError,
    colors: bool,
    output: &mut fmt::Formatter,
) {
    let file_name = "source";

    let out = Color::Fixed(81);
    let cache = (file_name, Source::from(source));

    let mut out_buf = Vec::new();

    Report::build(ReportKind::Error, file_name, 12)
        .with_code(code_for_error(&ty_error.it))
        .with_message(error_label(&ty_error.it, name_map))
        .with_label(
            Label::new((
                file_name,
                ty_error.at.start().into()..ty_error.at.end().into(),
            ))
            .with_message(error_label(&ty_error.it, name_map))
            .with_color(out),
        )
        .with_config(Config::default().with_color(colors))
        .finish()
        .write(cache, &mut out_buf)
        .unwrap();

    writeln!(output, "{}", str::from_utf8(&out_buf).unwrap()).unwrap();
}
