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
        highlight::highlight(&check_result.parse, &check_result.occurrences),
    )
    .into_iter()
    .map(|(start, end, kind)| Highlight::new(start, end, kind))
    .collect();
    let (ctx, result) = match check_result.ir {
        Some(ir) if check_result.errors.is_empty() => {
            let (wasm, ctx) = codegen(ir, check_result.names);
            (ctx, Ok(wasm))
        }
        _ => (check_result.names, Err(check_result.errors)),
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
            errors: errors
                .into_iter()
                .map(|e| Diagnostic {
                    message: e.message(&ctx),
                    start: e.at().start().into(),
                    end: e.at().end().into(),
                })
                .collect(),
        },
    }
}
