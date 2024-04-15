use line_index::{LineCol, LineIndex};
use parser::parse_prog;
use syntax::{ast::AstNode, nodes::Root};

pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod types;

fn pretty_line_col(line_col: &LineCol) -> String {
    format!("{}:{}", line_col.line, line_col.col)
}

pub fn check_program(source: &str) {
    let line_index = LineIndex::new(source);
    let parse = parse_prog(source);
    if !parse.errors().is_empty() {
        println!("Parse Errors:\n==========");
        for error in parse.errors() {
            // TODO: Translate into line/col
            println!(
                "{}-{}: {}",
                pretty_line_col(&line_index.line_col(error.1.start())),
                pretty_line_col(&line_index.line_col(error.1.end())),
                error.0
            )
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
