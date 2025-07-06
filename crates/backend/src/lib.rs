pub mod codegen;
mod wasm_builder;

use camino::Utf8PathBuf;
use codegen::codegen;
use frontend::run_frontend;

pub fn compile_program(sources: &[(Utf8PathBuf, String)]) -> Result<Vec<u8>, String> {
    let check_result = run_frontend(sources)?;

    if let Some(count) = check_result.display_errors() {
        return Err(format!("Compiling failed with {count} errors"));
    }

    let (ctx, ir) = check_result.consume();
    let (wasm, _) = codegen(ir, ctx);
    Ok(wasm)
}
