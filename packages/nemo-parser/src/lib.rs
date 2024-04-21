pub mod builtins;
pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod types;

use line_index::{LineCol, LineIndex};
use nemo_backend::{codegen::codegen, ir::NameMap};
use parser::{parse_prog, ParseError};
use std::fmt::{self, Write};
use syntax::{ast::AstNode, nodes::Root};
use types::{errors::TyError, CheckResult};

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
    ) -> DisplayCheckError<'src, 'err> {
        DisplayCheckError {
            source,
            error: self,
            name_map,
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
}

pub struct DisplayCheckError<'a, 'b> {
    error: &'b CheckError,
    source: &'a str,
    name_map: &'a NameMap,
}

impl fmt::Display for DisplayCheckError<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.error {
            CheckError::ParseError(err) => {
                write!(f, "{}", err.display(self.source))
            }
            CheckError::TypeError(err) => {
                write!(f, "{}", err.display(self.source, self.name_map))
            }
        }
    }
}

pub fn render_errors(errs: &[CheckError], source: &str, name_map: &NameMap) -> String {
    let mut output = String::new();
    for err in errs {
        writeln!(output, "{}", err.display(source, name_map)).unwrap();
    }
    output
}

pub fn run_frontend(source: &str) -> CheckResult<CheckError> {
    let parse = parse_prog(source);
    let mut errors = vec![];
    let check_result = match Root::cast(parse.syntax()) {
        None => panic!("Parse didn't yield a Root node"),
        Some(root) => types::check_prog(root),
    };

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
        name_map: check_result.name_map,
        typed_nodes: check_result.typed_nodes,
        names: check_result.names,
        ir: check_result.ir,
    }
}

pub fn check_program(source: &str) -> (NameMap, Vec<CheckError>) {
    let check_result = run_frontend(source);
    (check_result.name_map, check_result.errors)
}

pub fn compile_program(source: &str) -> (NameMap, Result<Vec<u8>, Vec<CheckError>>) {
    let check_result = run_frontend(source);
    // Maybe don't return Wasm if there are errors?
    if let Some(ir) = check_result.ir {
        let wasm = codegen(ir, &check_result.name_map);
        (check_result.name_map, Ok(wasm))
    } else {
        (check_result.name_map, Err(check_result.errors))
    }
}
