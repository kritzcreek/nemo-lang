pub mod codegen;
mod wasm_builder;

use codegen::codegen;
use frontend::run_frontend;

pub fn compile_program(source: &str) -> Result<Vec<u8>, String> {
    let check_result = run_frontend(source);

    if check_result.has_errors() {
        let mut count = 0;
        for err in check_result.errors() {
            count += 1;
            eprintln!("{}", err.display(source, &check_result.ctx, true));
        }
        return Err(format!("Compiling failed with {} errors", count));
    }

    let (ctx, ir) = check_result.consume();
    let (wasm, _) = codegen(ir.expect("No IR despite no check errors"), ctx);
    Ok(wasm)
}
