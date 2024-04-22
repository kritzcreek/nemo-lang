use tree_sitter_highlight::HighlightConfiguration;
use tree_sitter_highlight::HighlightEvent;
use tree_sitter_highlight::Highlighter;
use tree_sitter_nemo::{language, HIGHLIGHTS_QUERY};

pub const HIGHLIGHT_NAMES: [&str; 7] = [
    "keyword", "type", "function", "operator", "property", "number", "comment",
];

pub fn highlight(program: &str) -> Vec<HighlightEvent> {
    let nemo_language = language();

    let mut nemo_config =
        HighlightConfiguration::new(nemo_language, "nemo", HIGHLIGHTS_QUERY, "", "").unwrap();

    nemo_config.configure(&HIGHLIGHT_NAMES);

    let mut highlighter = Highlighter::new();

    let highlights = highlighter
        .highlight(&nemo_config, program.as_bytes(), None, |_| None)
        .unwrap();

    highlights.map(|e| e.unwrap()).collect()
}
