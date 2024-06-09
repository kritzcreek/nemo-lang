pub mod builtins;
pub mod highlight;
pub mod ir;
pub mod parser;
pub mod syntax;
pub mod types;

use crate::ir::{NameMap, NameSupply};
use line_index::{LineCol, LineIndex};
use parser::{parse_prog, ParseError};
use rowan::TextRange;
use std::fmt::{self, Write};
use types::{TyError, CheckResult};

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

pub fn render_errors(errs: &[CheckError], source: &str, name_map: &NameMap) -> String {
    let mut output = String::new();
    for err in errs {
        writeln!(output, "{}", err.display(source, name_map, true)).unwrap();
    }
    output
}

pub fn run_frontend(source: &str) -> CheckResult<CheckError> {
    let parse = parse_prog(source);
    let mut errors = vec![];
    let check_result = types::check_prog(parse.root());

    for error in parse.errors {
        errors.push(CheckError::ParseError(error));
    }
    for error in check_result.errors {
        errors.push(CheckError::TypeError(error));
    }
    if errors.is_empty() && check_result.ir.is_none() {
        panic!("No IR generated, despite no errors")
    };

    CheckResult {
        errors,
        names: check_result.names,
        typed_nodes: check_result.typed_nodes,
        occurrences: check_result.occurrences,
        ir: check_result.ir,
        parse: check_result.parse,
    }
}

pub fn check_program(source: &str) -> (NameSupply, Vec<CheckError>) {
    let check_result = run_frontend(source);
    (check_result.names, check_result.errors)
}
