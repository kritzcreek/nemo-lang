use nemo_frontend::{types::Typechecker, pretty::Printer};
use tree_sitter::Parser;

const EXAMPLE_PROG: &str = include_str!("../example.nemo");

pub fn main() {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_nemo::language()).unwrap();
    let tree = parser.parse(EXAMPLE_PROG, None).unwrap();
    let root_node = tree.root_node();

    let tc = Typechecker::new(EXAMPLE_PROG);
    match tc.infer_prog(root_node) {
        Ok(tl) => {
            let printer = Printer::new(true);
            println!("{}", printer.print_program(&tl))
        },
        Err(err) => println!("{err}"),
    };
    assert!(!root_node.to_sexp().contains("ERROR"))
}
