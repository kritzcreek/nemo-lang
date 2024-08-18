use backend::codegen::codegen;
use frontend::highlight;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(getter_with_clone)]
#[derive(Clone)]
pub struct Diagnostic {
    pub message: String,
    pub start: usize,
    pub end: usize,
}

#[wasm_bindgen(getter_with_clone)]
#[derive(Clone)]
pub struct Highlight {
    pub start: usize,
    pub end: usize,
    // TODO wasteful
    pub kind: String,
}

impl Highlight {
    pub fn new(start: usize, end: usize, kind: highlight::HighlightKind) -> Self {
        Highlight {
            kind: format!("{:?}", kind),
            start,
            end,
        }
    }
}

#[wasm_bindgen(getter_with_clone)]
pub struct CompileResult {
    pub wasm: Vec<u8>,
    pub wast: String,
    pub errors: Vec<Diagnostic>,
    pub highlights: Vec<Highlight>,
}

#[wasm_bindgen]
pub fn compile(input: &str) -> CompileResult {
    console_error_panic_hook::set_once();
    let check_result = frontend::run_frontend(input);
    let highlights = highlight::translate_to_utf16(
        input,
        vec![],
        // highlight::highlight(&check_result.parse, &check_result.occurrences),
    )
    .into_iter()
    .map(|(start, end, kind)| Highlight::new(start, end, kind))
    .collect();
    let result = if check_result.has_errors() {
        let errors = check_result
            .errors()
            .map(|e| Diagnostic {
                message: e.message(&check_result.ctx),
                start: e.at().start().into(),
                end: e.at().end().into(),
            })
            .collect();
        Err(errors)
    } else {
        let (ctx, ir) = check_result.consume();
        let (wasm, _) = codegen(ir.expect("No IR despite no errors"), ctx);
        Ok(wasm)
    };

    match result {
        Ok(wasm) => {
            let wast =
                wasmprinter::print_bytes(&wasm).unwrap_or_else(|e| format!("internal error: {e}"));
            CompileResult {
                wasm,
                wast,
                highlights,
                errors: vec![],
            }
        }
        Err(errors) => CompileResult {
            wasm: vec![],
            wast: "".to_string(),
            highlights,
            errors,
        },
    }
}
