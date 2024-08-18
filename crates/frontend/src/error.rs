use crate::types::TyError;
use crate::{ir::Ctx, parser::ParseError};
use line_index::{LineCol, LineIndex};
use std::fmt;
use text_size::TextRange;

#[derive(Debug, Clone)]
pub enum CheckError<'a> {
    ParseError(&'a ParseError),
    TypeError(&'a TyError),
}

impl<'a> CheckError<'a> {
    pub fn display<'src, 'err>(
        &'err self,
        source: &'src str,
        ctx: &'src Ctx,
        colors: bool,
    ) -> DisplayCheckError<'src, 'err> {
        DisplayCheckError {
            source,
            error: self.clone(),
            ctx,
            colors,
        }
    }

    pub fn line_col(&self, line_index: &LineIndex) -> (LineCol, LineCol) {
        match self {
            CheckError::ParseError(err) => err.line_col(line_index),
            CheckError::TypeError(err) => err.line_col(line_index),
        }
    }

    pub fn message(&self, ctx: &Ctx) -> String {
        match self {
            CheckError::ParseError(err) => err.it.clone(),
            CheckError::TypeError(err) => err.message(ctx),
        }
    }

    pub fn at(&self) -> TextRange {
        match self {
            CheckError::ParseError(err) => err.at,
            CheckError::TypeError(err) => err.at,
        }
    }
}

pub struct DisplayCheckError<'a, 'b> {
    error: CheckError<'b>,
    source: &'a str,
    ctx: &'a Ctx,
    colors: bool,
}

impl fmt::Display for DisplayCheckError<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.error {
            CheckError::ParseError(err) => {
                write!(f, "{}", err.display(self.source, self.colors))
            }
            CheckError::TypeError(err) => {
                write!(f, "{}", err.display(self.source, self.ctx, self.colors))
            }
        }
    }
}
