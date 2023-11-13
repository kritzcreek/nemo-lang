use nemo_backend::{codegen::codegen, lower::lower};
use nemo_frontend::pretty::Printer;
use std::fs;

const EXAMPLE_PROG: &str = include_str!("../../../example.nemo");

pub fn main() {
    match nemo_frontend::check_program(EXAMPLE_PROG) {
        Ok(program) => {
            let printer = Printer::new(true);
            println!("{}", printer.print_program(&program));
            let (lowered, name_map) = lower(program);
            let bytes = codegen(lowered, name_map);
            fs::write("program.wasm", bytes).unwrap();
        }
        Err(err) => println!("{err}"),
    };
}
