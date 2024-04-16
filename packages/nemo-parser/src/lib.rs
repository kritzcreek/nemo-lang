use parser::parse_prog;
use syntax::{ast::AstNode, nodes::Root};
use types::errors::TyError;

pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod types;

pub fn check_program(source: &str) -> Vec<TyError> {
    let parse = parse_prog(source);
    // println!("{}", parse.debug_tree());

    let check_result = match Root::cast(parse.syntax()) {
        None => panic!("Parse didn't yield a Root node"),
        Some(root) => types::check_prog(root),
    };

    // println!("{:?}", check_result.names);
    // println!("{:?}", check_result.typed_nodes);
    // println!("{:?}", check_result.errors);
    check_result.errors
}
