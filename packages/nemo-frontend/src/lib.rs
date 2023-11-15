pub mod builtins;
pub mod parser;
pub mod pretty;
pub mod syntax;
pub mod types;

use parser::parse_program;
use types::typecheck;

pub fn check_program(program: &str) -> types::TyResult<syntax::Program> {
    let parse_tree = parse_program(program);
    typecheck(program, parse_tree.root_node())
}
