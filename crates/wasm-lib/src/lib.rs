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

const STDLIB: &[(&str, &str)] = &[
    ("std/option.nemo", include_str!("../../../std/option.nemo")),
    ("std/result.nemo", include_str!("../../../std/result.nemo")),
    ("std/string.nemo", include_str!("../../../std/string.nemo")),
    ("std/io.nemo", include_str!("../../../std/io.nemo")),
    ("std/byte.nemo", include_str!("../../../std/byte.nemo")),
    (
        "std/canvas.nemo",
        include_str!("../../../playground/examples/canvas.nemo"),
    ),
];

#[wasm_bindgen]
pub fn compile(input: &str) -> CompileResult {
    console_error_panic_hook::set_once();
    let mut sources = vec![];
    for (path, source) in STDLIB {
        sources.push((Utf8Path::new(path).to_path_buf(), source.to_string()))
    }
    let input_path = Utf8Path::new("input");
    sources.push((input_path.to_path_buf(), input.to_string()));
    // TODO: Report this properly
    let check_result = frontend::run_frontend(&sources).unwrap();
    let mut highlights = vec![];
    for module in &check_result.modules {
        if module.parse_result.path != input_path {
            continue;
        }
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
        let (wasm, _) = codegen(ir, ctx);
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
