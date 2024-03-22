pub mod builtins;
pub mod parser;
pub mod pretty;
pub mod syntax;
pub mod type_errors;
pub mod types;
pub mod types_copy;
mod ast;

use parser::parse_program;
use types::typecheck;

pub fn check_program(source: &str) -> types::TyResult<syntax::Program> {
    let parse_tree = parse_program(source);
    typecheck(source, parse_tree.root_node())
}

pub fn print_program(program: &syntax::Program) -> String {
    let printer = pretty::Printer::new(true);
    printer.print_program(program)
}
