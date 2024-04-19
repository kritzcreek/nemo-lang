use nemo_backend::codegen::{self, codegen};
use parser::{parse_prog, ParseError};
use syntax::{ast::AstNode, nodes::Root};
use types::errors::TyError;

pub mod builtins;
pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod types;

pub fn check_program(source: &str) -> (Vec<ParseError>, Vec<TyError>) {
    let parse = parse_prog(source);
    let parse_errors = parse.errors().to_vec();
    // println!("{}", parse.debug_tree());

    let check_result = match Root::cast(parse.syntax()) {
        None => panic!("Parse didn't yield a Root node"),
        Some(root) => types::check_prog(root),
    };

    (parse_errors, check_result.errors)
}

pub fn compile_program(source: &str) -> Option<Vec<u8>> {
    let parse = parse_prog(source);
    let parse_errors = parse.errors().to_vec();
    // println!("{}", parse.debug_tree());

    let check_result = match Root::cast(parse.syntax()) {
        None => panic!("Parse didn't yield a Root node"),
        Some(root) => types::check_prog(root),
    };

    if parse_errors.is_empty() && check_result.errors.is_empty() {
        Some(codegen(check_result.ir?, check_result.name_map))
    } else {
        None
    }
}
