use crate::lexer::SyntaxKind;
use crate::syntax::ast::AstNode;
use backend::ir::Name;
use rowan::TextRange;

use crate::syntax::nodes::Root;
use crate::types::check::OccurenceMap;
use crate::T;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightKind {
    Function,
    Local,
    Global,
    Type,
    Keyword,
    Literal,
    Operator,
    Comment,
    Property,
}

#[derive(Debug, Clone)]
pub struct Highlight {
    pub range: TextRange,
    pub kind: HighlightKind,
}

pub fn highlight(root: &Root, occurrences: &OccurenceMap) -> Vec<Highlight> {
    let mut highlights = Vec::new();
    for node in root.syntax().descendants_with_tokens() {
        match node.kind() {
            T![let]
            | T![set]
            | T![global]
            | T![match]
            | T![if]
            | T![else]
            | T![while]
            | T![fn]
            | T![variant]
            | T![struct]
            | T![import]
            | T![from] => {
                highlights.push(Highlight {
                    range: node.text_range(),
                    kind: HighlightKind::Keyword,
                });
            }
            T![true] | T![false] | T![int_lit] | T![float_lit] => {
                highlights.push(Highlight {
                    range: node.text_range(),
                    kind: HighlightKind::Literal,
                });
            }
            T![+] | T![-] | T![*] | T![/] | T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=] => {
                highlights.push(Highlight {
                    range: node.text_range(),
                    kind: HighlightKind::Operator,
                });
            }
            T![i32] | T![bool] | T![f32] | T![unit] => {
                highlights.push(Highlight {
                    range: node.text_range(),
                    kind: HighlightKind::Type,
                });
            }
            SyntaxKind::LINE_COMMENT => {
                highlights.push(Highlight {
                    range: node.text_range(),
                    kind: HighlightKind::Comment,
                });
            }
            _ => continue,
        }
    }

    for (ptr, occurrence) in occurrences {
        let name = *occurrence.name();
        match name {
            Name::Global(_) => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Global,
                });
            }
            Name::Local(_) => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Local,
                });
            }
            Name::Func(_) => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Function,
                });
            }
            Name::Type(_) => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Type,
                });
            }
            Name::Field(_) => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Property,
                });
            }
            Name::Gen(_) => {}
        }
    }
    highlights.sort_by_key(|hl| hl.range.start());
    highlights
}
