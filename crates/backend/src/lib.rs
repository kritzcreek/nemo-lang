pub mod codegen;
mod wasm_builder;

use codegen::codegen;
use frontend::run_frontend;

pub fn compile_program(source: &str) -> Result<Vec<u8>, String> {
    let check_result = run_frontend(source);

    if !check_result.errors.is_empty() {
        for err in &check_result.errors {
            eprintln!("{}", err.display(source, &check_result.names, true));
        }
        return Err(format!(
            "Compiling failed with {} errors",
            check_result.errors.len()
        ));
    }

    let (wasm, _) = codegen(
        check_result.ir.expect("No IR despite no check errors"),
        check_result.names,
    );
    Ok(wasm)
}
