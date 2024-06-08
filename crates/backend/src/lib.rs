pub mod codegen;
mod wasm_builder;

use codegen::codegen;
use frontend::{ir::NameSupply, run_frontend, CheckError};

pub fn compile_program(source: &str) -> (NameSupply, Result<Vec<u8>, Vec<CheckError>>) {
    let check_result = run_frontend(source);
    match check_result.ir {
        Some(ir) if check_result.errors.is_empty() => {
            let (wasm, names) = codegen(ir, check_result.names);
            (names, Ok(wasm))
        }
        _ => (check_result.names, Err(check_result.errors)),
    }
}
