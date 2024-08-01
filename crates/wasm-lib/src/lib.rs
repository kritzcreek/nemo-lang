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
    pub fn new(input: &str, highlight: highlight::Highlight) -> Self {
        Highlight {
            kind: format!("{:?}", highlight.kind),
            start: utf8_offset_to_utf16_offset(input, highlight.range.start().into()),
            end: utf8_offset_to_utf16_offset(input, highlight.range.end().into()),
        }
    }
}

// TODO: This is quadratic, do it in linear time
fn utf8_offset_to_utf16_offset(s: &str, byte_offset: usize) -> usize {
    s.char_indices()
        .take_while(|(i, _)| *i < byte_offset)
        .map(|(_, c)| c.len_utf16())
        .sum()
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
    let highlights = highlight::highlight(&check_result.parse, &check_result.occurrences)
        .into_iter()
        .map(|h| Highlight::new(input, h))
        .collect();
    let (name_map, result) = match check_result.ir {
        Some(ir) if check_result.errors.is_empty() => {
            let (wasm, names) = codegen(ir, check_result.names);
            (names.name_map, Ok(wasm))
        }
        _ => (check_result.names.name_map, Err(check_result.errors)),
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
                    message: e.message(&name_map),
                    start: e.at().start().into(),
                    end: e.at().end().into(),
                })
                .collect(),
        },
    }
}
