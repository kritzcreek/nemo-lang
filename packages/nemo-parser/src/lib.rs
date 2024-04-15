use parser::parse_prog;
use syntax::{ast::AstNode, nodes::Root};

pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod types;

pub fn check_program(source: &str) {
    let parse = parse_prog(source);
    if !parse.errors().is_empty() {
        println!("Parse Errors:\n==========");
        for error in parse.errors() {
            // TODO: Translate into line/col
            println!("{:?}: {}", error.1, error.0)
        }
    }

    // println!("{}", parse.debug_tree());

    let check_result = match Root::cast(parse.syntax()) {
        None => panic!("Parse didn't yield a Root node"),
        Some(root) => types::check_prog(root),
    };

    // println!("{:?}", check_result.names);
    println!("{:?}", check_result.typed_nodes);
    println!("{:?}", check_result.errors);
}
