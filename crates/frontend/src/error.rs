use crate::ir::NameMap;
use crate::parser::ParseError;
use crate::types::TyError;
use line_index::{LineCol, LineIndex};
use std::fmt;
use text_size::TextRange;

#[derive(Debug)]
pub enum CheckError {
    ParseError(ParseError),
    TypeError(TyError),
}

impl CheckError {
    pub fn display<'src, 'err>(
        &'err self,
        source: &'src str,
        name_map: &'src NameMap,
        colors: bool,
    ) -> DisplayCheckError<'src, 'err> {
        DisplayCheckError {
            source,
            error: self,
            name_map,
            colors,
        }
    }

    pub fn line_col(&self, line_index: &LineIndex) -> (LineCol, LineCol) {
        match self {
            CheckError::ParseError(err) => err.line_col(line_index),
            CheckError::TypeError(err) => err.line_col(line_index),
        }
    }

    pub fn message(&self, name_map: &NameMap) -> String {
        match self {
            CheckError::ParseError(err) => err.it.clone(),
            CheckError::TypeError(err) => err.message(name_map),
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
    error: &'b CheckError,
    source: &'a str,
    name_map: &'a NameMap,
    colors: bool,
}

impl fmt::Display for DisplayCheckError<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.error {
            CheckError::ParseError(err) => {
                write!(f, "{}", err.display(self.source, self.colors))
            }
            CheckError::TypeError(err) => {
                write!(
                    f,
                    "{}",
                    err.display(self.source, self.name_map, self.colors)
                )
            }
        }
    }
}
