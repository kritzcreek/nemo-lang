use crate::ir::NameTag;
use crate::parser::SyntaxKind;
use crate::syntax::{AstNode, Module};
use crate::types::OccurrenceMap;
use crate::T;
use text_size::TextRange;

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

pub fn highlight(module: &Module, occurrences: &OccurrenceMap) -> Vec<Highlight> {
    let mut highlights = Vec::new();
    for node in module.syntax().descendants_with_tokens() {
        match node.kind() {
            T![let]
            | T![set]
            | T![global]
            | T![match]
            | T![when]
            | T![if]
            | T![else]
            | T![while]
            | T![return]
            | T![fn]
            | T![variant]
            | T![struct]
            | T![import]
            | T![use]
            | T![module]
            | T![exports]
            | T![from] => {
                highlights.push(Highlight {
                    range: node.text_range(),
                    kind: HighlightKind::Keyword,
                });
            }
            T![true]
            | T![false]
            | T![int_lit]
            | T![binary_lit]
            | T![hex_lit]
            | T![float_lit]
            | T![bytes_lit] => {
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
            T![I32] | T![U32] | T![F32] | T![Bool] | T![Unit] => {
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
        match name.tag {
            NameTag::Global => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Global,
                });
            }
            NameTag::Local => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Local,
                });
            }
            NameTag::Func => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Function,
                });
            }
            NameTag::Type | NameTag::TypeVar => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Type,
                });
            }
            NameTag::Field => {
                highlights.push(Highlight {
                    range: ptr.0,
                    kind: HighlightKind::Property,
                });
            }
            NameTag::Gen => {}
        }
    }
    highlights.sort_by_key(|hl| hl.range.start());
    highlights
}

pub fn translate_to_utf16(
    input: &str,
    highlights: Vec<Highlight>,
) -> Vec<(usize, usize, HighlightKind)> {
    let mut prev_tkn_end = 0;
    let mut utf16_offset = 0;
    highlights
        .into_iter()
        .map(|h| {
            let tkn_start = h.range.start().into();
            let tkn_end = h.range.end().into();
            utf16_offset += str::encode_utf16(&input[prev_tkn_end..tkn_start]).count();
            let utf16_start = utf16_offset;
            utf16_offset += str::encode_utf16(&input[tkn_start..tkn_end]).count();
            prev_tkn_end = tkn_end;
            (utf16_start, utf16_offset, h.kind)
        })
        .collect()
}
