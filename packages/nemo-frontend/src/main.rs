use nemo_frontend::{parser::parse_program, types::typecheck};

fn main() {
    let source = "let x : i32 = 10.0";
    let parsed = parse_program(&source);
    typecheck(source, parsed.root_node()).unwrap();
}
