pub mod codegen;
pub mod ir;
pub mod lower;
mod wasm_builder;

use codegen::codegen;
use lower::lower;
use nemo_frontend::check_program;

pub fn compile_program(source: &str) -> Vec<u8> {
    // TODO: Report errors here
    let checked = check_program(source).unwrap();
    let (program, name_map) = lower(checked);
    codegen(program, name_map)
}
