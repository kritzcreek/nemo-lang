pub mod builtins;
pub mod parser;
pub mod pretty;
pub mod syntax;
pub mod types;

use parser::parse_program;

pub fn check_program(program: &str) -> types::TyResult<syntax::Program> {
    let parse_tree = parse_program(program);
    let tc = types::Typechecker::new(program);
    tc.infer_prog(parse_tree.root_node())
}
