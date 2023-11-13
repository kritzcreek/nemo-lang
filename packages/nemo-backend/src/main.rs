use nemo_backend::{codegen::codegen, lower::lower};
use nemo_frontend::pretty::Printer;
use std::{
    fs,
    io::{stdout, Read, Write},
};

pub fn main() {
    let mut code = Vec::new();
    let stdin = std::io::stdin();
    let mut handle = stdin.lock();
    handle.read_to_end(&mut code).unwrap();

    match nemo_frontend::check_program(&String::from_utf8_lossy(&code)) {
        Ok(program) => {
            let printer = Printer::new(true);
            eprintln!("{}", printer.print_program(&program));
            let (lowered, name_map) = lower(program);
            let bytes = codegen(lowered, name_map);
            fs::write("bytes.wasm", &bytes).unwrap();
            stdout().write_all(&bytes).unwrap();
            stdout().flush().unwrap();
        }
        Err(err) => eprintln!("{err}"),
    };
}
