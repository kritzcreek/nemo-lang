pub mod builtins;
pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod types;

use nemo_backend::{codegen::codegen, ir};
use parser::{parse_prog, ParseError};
use std::fmt::{self, Write};
use syntax::{ast::AstNode, nodes::Root};
use types::errors::TyError;

#[derive(Debug)]
pub enum CheckError {
    ParseError(ParseError),
    TypeError(TyError),
}

impl CheckError {
    pub fn display<'src, 'err>(&'err self, source: &'src str) -> DisplayCheckError<'src, 'err> {
        DisplayCheckError {
            source,
            error: self,
        }
    }
}

pub struct DisplayCheckError<'a, 'b> {
    source: &'a str,
    error: &'b CheckError,
}

impl fmt::Display for DisplayCheckError<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.error {
            CheckError::ParseError(err) => {
                write!(f, "{}", err.display(self.source))
            }
            CheckError::TypeError(err) => {
                write!(f, "{}", err.display(self.source))
            }
        }
    }
}

pub fn render_errors(errs: &[CheckError], source: &str) -> String {
    let mut output = String::new();
    for err in errs {
        writeln!(output, "{}", err.display(source)).unwrap();
    }
    output
}

pub fn run_frontend(source: &str) -> Result<(ir::Program, ir::NameMap), Vec<CheckError>> {
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
    if !errors.is_empty() {
        return Err(errors);
    }
    let Some(ir) = check_result.ir else {
        panic!("No IR generated, despite no errors")
    };
    Ok((ir, check_result.name_map))
}

pub fn check_program(source: &str) -> Vec<CheckError> {
    run_frontend(source).err().unwrap_or_default()
}

pub fn compile_program(source: &str) -> Result<Vec<u8>, Vec<CheckError>> {
    run_frontend(source).map(|(ir, name_map)| codegen(ir, name_map))
}
