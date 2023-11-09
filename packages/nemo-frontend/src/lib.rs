pub mod builtins;
pub mod pretty;
pub mod syntax;
pub mod types;

use tree_sitter::Parser;

pub fn check_program(program: &str) -> types::TyResult<syntax::Program> {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_nemo::language()).unwrap();
    let tree = parser.parse(program, None).unwrap();

    let tc = types::Typechecker::new(program);
    tc.infer_prog(tree.root_node())
}
