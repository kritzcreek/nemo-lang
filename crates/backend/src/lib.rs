pub mod codegen;
mod wasm_builder;

use camino::Utf8PathBuf;
use codegen::codegen;
use frontend::run_frontend_new;

pub fn compile_program(sources: &[(Utf8PathBuf, String)]) -> Result<Vec<u8>, String> {
    let check_result = run_frontend_new(sources);

    if let Some(count) = check_result.display_errors() {
        return Err(format!("Compiling failed with {} errors", count));
    }

    let (ctx, ir) = check_result.consume();
    let (wasm, _) = codegen(ir.expect("No IR despite no check errors"), ctx);
    Ok(wasm)
}
