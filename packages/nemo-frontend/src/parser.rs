use tree_sitter::{Parser, Tree};
use tree_sitter_highlight::HighlightConfiguration;
use tree_sitter_highlight::HighlightEvent;
use tree_sitter_highlight::Highlighter;

const HIGHLIGHT_NAMES: [&'static str; 7] = [
    "keyword", "type", "function", "operator", "property", "number", "comment",
];

pub fn parse_program(program: &str) -> Tree {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_nemo::language()).unwrap();
    parser.parse(program, None).unwrap()
}

pub fn highlight(program: &str) -> Vec<HighlightEvent> {
    let nemo_language = tree_sitter_nemo::language();

    let mut nemo_config =
        HighlightConfiguration::new(nemo_language, tree_sitter_nemo::HIGHLIGHTS_QUERY, "", "")
            .unwrap();

    nemo_config.configure(&HIGHLIGHT_NAMES);

    let mut highlighter = Highlighter::new();

    let highlights = highlighter
        .highlight(&nemo_config, program.as_bytes(), None, |_| None)
        .unwrap();

    return highlights.map(|e| e.unwrap()).collect();
    // for event in highlights {
    //     match event.unwrap() {
    //         HighlightEvent::Source { start, end } => {
    //             eprintln!("source: {}-{}", start, end);
    //         }
    //         HighlightEvent::HighlightStart(s) => {
    //             let name = HIGHLIGHT_NAMES[s.0];
    //             eprintln!("highlight style started: {}", name);
    //         }
    //         HighlightEvent::HighlightEnd => {
    //             eprintln!("highlight style ended");
    //         }
    //     }
    // }
}
