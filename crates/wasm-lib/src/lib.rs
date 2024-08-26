use backend::codegen::codegen;
use camino::Utf8Path;
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
    let sources = vec![(Utf8Path::new("input").to_path_buf(), input.to_string())];
    let check_result = frontend::run_frontend(&sources);
    let mut highlights = vec![];
    for module in &check_result.modules {
        highlights.extend(
            highlight::translate_to_utf16(
                input,
                highlight::highlight(&module.parse_result.parse, &module.occurrences),
            )
            .into_iter()
            .map(|(start, end, kind)| Highlight::new(start, end, kind)),
        );
    }
    let errors: Vec<Diagnostic> = check_result
        .errors()
        .map(|e| Diagnostic {
            message: e.message(&check_result.ctx),
            start: e.at().start().into(),
            end: e.at().end().into(),
        })
        .collect();
    let result = if !errors.is_empty() {
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
