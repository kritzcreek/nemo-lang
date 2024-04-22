use wasm_bindgen::prelude::*;

#[wasm_bindgen(getter_with_clone)]
#[derive(Clone)]
pub struct Diagnostic {
    pub message: String,
    pub start: usize,
    pub end: usize,
}

#[wasm_bindgen(getter_with_clone)]
pub struct CompileResult {
    pub wasm: Vec<u8>,
    pub errors: Vec<Diagnostic>,
}

#[wasm_bindgen]
pub fn compile(input: &str) -> CompileResult {
    let (name_map, result) = frontend::compile_program(&input);
    match result {
        Ok(wasm) => CompileResult {
            wasm,
            errors: vec![],
        },
        Err(errors) => CompileResult {
            wasm: vec![],
            errors: errors
                .into_iter()
                .map(|e| Diagnostic {
                    message: format!("{}", e.message(&name_map)),
                    start: e.at().start().into(),
                    end: e.at().end().into(),
                })
                .collect(),
        },
    }
}
