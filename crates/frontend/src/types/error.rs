use crate::ir::{Ctx, Name, Ty};
use crate::syntax::{AstNode, SyntaxNode, SyntaxToken};
use ariadne::{Config, Label, Report, ReportKind, Source};
use camino::Utf8Path;
use core::fmt;
use line_index::{LineCol, LineIndex};
use std::str;
use text_size::TextRange;

#[derive(Debug)]
pub struct TyErrors {
    pub errors: Vec<TyError>,
}

impl TyErrors {
    pub fn new() -> Self {
        Self { errors: vec![] }
    }

    pub fn report<N: HasRange>(&mut self, node: &N, error: TyErrorData) {
        self.errors.push(TyError {
            at: node.to_range(),
            it: error,
        })
    }
}

pub trait HasRange {
    fn to_range(&self) -> TextRange;
}

impl<N: AstNode> HasRange for N {
    fn to_range(&self) -> TextRange {
        self.syntax().to_range()
    }
}

impl HasRange for SyntaxNode {
    fn to_range(&self) -> TextRange {
        let at = self.text_range();
        let mut start = at.start();
        let mut end = at.end();
        let mut children = self.descendants_with_tokens();
        for elem in children.by_ref() {
            if elem.as_token().is_some() && !elem.kind().is_whitespace() {
                start = elem.text_range().start();
                break;
            }
        }

        for elem in children {
            if elem.as_token().is_some() && !elem.kind().is_whitespace() {
                end = elem.text_range().end();
            }
        }

        TextRange::new(start, end)
    }
}

impl HasRange for SyntaxToken {
    fn to_range(&self) -> TextRange {
        self.text_range()
    }
}

impl HasRange for TextRange {
    fn to_range(&self) -> TextRange {
        *self
    }
}

#[derive(Debug)]
pub struct TyError {
    pub at: TextRange,
    pub it: TyErrorData,
}

impl TyError {
    pub fn display<'err, 'src>(
        &'err self,
        ctx: &'src Ctx,
        path: &'src Utf8Path,
        source: &'src str,
        colors: bool,
    ) -> TyErrorDisplay<'src, 'err> {
        TyErrorDisplay {
            source,
            path,
            ctx,
            ty_error: self,
            colors,
        }
    }

    pub fn line_col(&self, line_index: &LineIndex) -> (LineCol, LineCol) {
        let start = line_index.line_col(self.at.start());
        let end = line_index.line_col(self.at.end());
        (start, end)
    }

    pub fn message(&self, ctx: &Ctx) -> String {
        error_label(&self.it, ctx)
    }
}

pub struct TyErrorDisplay<'src, 'err> {
    ty_error: &'err TyError,
    source: &'src str,
    path: &'src Utf8Path,
    ctx: &'src Ctx,
    colors: bool,
}

impl fmt::Display for TyErrorDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        render_ty_error(
            self.ctx,
            self.path,
            self.source,
            self.ty_error,
            self.colors,
            f,
        );
        Ok(())
    }
}

#[derive(Debug)]
pub enum TyErrorData {
    MissingNode(String),
    InvalidLiteral,
    CantInferEmptyArray,
    CantInferLambda,
    CantInstantiateFunctionRef,
    TypeParamInVariantStruct,
    CantReturnFromGlobal,
    InvalidOperator(String, Ty, Ty),
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
    CantReassignCapturedVariable(Name),
    ArgCountMismatch(usize, usize),
    TyArgCountMismatch(usize, usize),
    CantInferTypeParam(Name),
    FieldTypeMismatch {
        struct_name: Name,
        field_name: Name,
        expected: Ty,
        actual: Ty,
    },
    UnknownAlternative {
        variant_name: Name,
        alternative: String,
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
    PatternTypeMismatch {
        expected: Ty,
    },
}

fn code_for_error(err_data: &TyErrorData) -> i32 {
    match err_data {
        TyErrorData::MissingNode(_) => 1,
        TyErrorData::InvalidLiteral => 2,
        TyErrorData::InvalidOperator(_, _, _) => 3,
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
        TyErrorData::UnknownAlternative { .. } => 19,
        TyErrorData::PatternTypeMismatch { .. } => 20,
        TyErrorData::CantInstantiateFunctionRef => 21,
        TyErrorData::TyArgCountMismatch(_, _) => 22,
        TyErrorData::TypeParamInVariantStruct => 23,
        TyErrorData::CantReturnFromGlobal => 24,
        TyErrorData::CantReassignCapturedVariable(_) => 25,
        TyErrorData::CantInferTypeParam(_) => 26,
        TyErrorData::CantInferLambda => 27,
    }
}

fn error_label(err_data: &TyErrorData, ctx: &Ctx) -> String {
    match err_data {
        TyErrorData::MissingNode(n) => format!("Internal/Parse error: Expected a node labeled {n}"),
        TyErrorData::InvalidLiteral => "Invalid literal couldn't be parsed".to_string(),
        TyErrorData::InvalidOperator(op, lhs, rhs) => format!("Invalid operator {} for lhs of type {} and rhs of type {}", op,
            lhs.display(ctx),
            rhs.display(ctx)
        ),
        TyErrorData::CantInferEmptyArray => "Can't infer type of an empty array".to_string(),
        TyErrorData::CantInferLambda => "Can't infer type of unannotated lambda".to_string(),
        TyErrorData::CantInstantiateFunctionRef => "Can't instantiate function reference. Only top-level functions may be polymorphic at this time.".to_string(),
        TyErrorData::CantReturnFromGlobal => "Can't 'return' from a global definition.".to_string(),
        TyErrorData::UnknownVar(v) => format!("Unknown variable {v}"),
        TyErrorData::UnknownFunction(f) => format!("Unknown function {f}"),
        TyErrorData::UnknownType(t) => format!("Unknown type {t}"),
        TyErrorData::UnknownIntrinsic(f, arg_count) => {
            format!("Unknown intrinsic {f} with argcount: {arg_count}")
        }
        TyErrorData::NonArrayIdx(ty) => format!(
            "Tried to index into a non-array type {}",
            ty.display(ctx)
        ),
        TyErrorData::NonStructIdx(ty) => format!(
            "Tried to index into a non-struct type {}",
            ty.display(ctx)
        ),
        TyErrorData::NotAFunction(ty) => format!(
            "Can't a call a value of type {} as a function",
            ty.display(ctx)
        ),
        TyErrorData::NonFunctionImport { name, ty } => format!(
            "Can't import a non-function value. {} is of type {}",
            ctx.display_name(*name),
            ty.display(ctx)
        ),
        TyErrorData::ArgCountMismatch(expected, actual) => format!(
            "Mismatched arg count. Expected {expected} {}, but got {actual}",
            if *expected == 1 {
                "argument"
            } else {
                "arguments"
            }
        ),
        TyErrorData::TyArgCountMismatch(expected, actual) => format!(
            "Mismatched type arg count. Expected {expected} {}, but got {actual}",
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
            ctx.display_name(*struct_name),
            ctx.display_name(*field_name),
            expected.display(ctx),
            actual.display(ctx)
        ),
        TyErrorData::UnknownAlternative {
            variant_name,
            alternative,
        } => format!(
            "Unknown alternative. {} does not have an alternative named {alternative}",
                ctx.display_name(*variant_name),
        ),
        TyErrorData::UnknownField {
            struct_name,
            field_name,
        } => format!(
            "Unknown field. {} does not have a field named {field_name}",
            ctx.display_name(*struct_name)
        ),
        TyErrorData::MissingField {
            struct_name,
            field_name,
        } => format!(
            "Missing field. {}.{} was not provided",
            ctx.display_name(*struct_name),
            ctx.display_name(*field_name)
        ),
        TyErrorData::TypeMismatch { expected, actual } => format!(
            "Type mismatch. Expected {}, but got {}",
            expected.display(ctx),
            actual.display(ctx)
        ),
        TyErrorData::PatternTypeMismatch { expected } => format!(
            "This pattern can't match a value of type {}",
            expected.display(ctx),
        ),
        TyErrorData::TypeParamInVariantStruct => {
            "Can't declare type parameters for structs in a variant.".to_string()
        }
        TyErrorData::CantReassignCapturedVariable(n) => {
            format!("Can't reassign the captured variable '{}'. Maybe you want to box this variable in a struct?", ctx.display_name(*n))
        }
        TyErrorData::CantInferTypeParam(n) => {
            format!("Can't infer type parameter '{}'. Explicitly supply the instantiation", ctx.display_name(*n))
        },
    }
}

pub fn render_ty_error(
    ctx: &Ctx,
    path: &Utf8Path,
    source: &str,
    ty_error: &TyError,
    colors: bool,
    output: &mut fmt::Formatter,
) {
    let cache = (path, Source::from(source));
    let mut out_buf = Vec::new();

    Report::build(ReportKind::Error, (path, ty_error.at.into()))
        .with_code(code_for_error(&ty_error.it))
        .with_message(error_label(&ty_error.it, ctx))
        .with_label(
            Label::new((path, ty_error.at.start().into()..ty_error.at.end().into()))
                .with_message(error_label(&ty_error.it, ctx)),
        )
        .with_config(Config::default().with_color(colors))
        .finish()
        .write(cache, &mut out_buf)
        .unwrap();

    write!(output, "{}", str::from_utf8(&out_buf).unwrap()).unwrap();
}
