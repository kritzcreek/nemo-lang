use crate::ir::Program;
use wasm_encoder::Module;

pub fn codegen(program: Program) -> Vec<u8> {
    let module = Module::new();
    module.finish()
}
