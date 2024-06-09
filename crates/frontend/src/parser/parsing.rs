use super::error::ParseError;
use super::lexer::{self, SyntaxKind, TToken};
use crate::syntax::NemoLanguage;
use rowan::{Checkpoint, GreenNodeBuilder, Language};
use std::str;
use text_size::{TextRange, TextSize};

pub struct Parser<'a> {
    tokens: Vec<TToken<'a>>,
    pub(crate) builder: GreenNodeBuilder<'static>,
    // TODO: Be smarter here
    pub(crate) errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut tkns = lexer::Lexer::lex(input);
        tkns.reverse();
        Self {
            tokens: tkns,
            builder: GreenNodeBuilder::new(),
            errors: vec![],
        }
    }

    pub(crate) fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(NemoLanguage::kind_to_raw(kind));
    }

    pub(crate) fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder
            .start_node_at(checkpoint, NemoLanguage::kind_to_raw(kind));
    }

    pub(crate) fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    pub(crate) fn finish_at(&mut self, c: Checkpoint, kind: SyntaxKind) {
        self.start_node_at(c, kind);
        self.finish_node();
    }

    pub(crate) fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
    }

    pub(crate) fn bump_any(&mut self) {
        let TToken {
            leading,
            token,
            trailing,
        } = self.tokens.pop().unwrap();

        for tkn in leading {
            if tkn.kind == SyntaxKind::LEX_ERROR {
                self.errors.push(ParseError {
                    it: "lexing error".to_string(),
                    at: TextRange::new(
                        TextSize::from(tkn.span.start as u32),
                        TextSize::from(tkn.span.end as u32),
                    ),
                })
            }
            self.builder
                .token(NemoLanguage::kind_to_raw(tkn.kind), tkn.text);
        }

        self.builder
            .token(NemoLanguage::kind_to_raw(token.kind), token.text);

        for tkn in trailing {
            if tkn.kind == SyntaxKind::LEX_ERROR {
                self.errors.push(ParseError {
                    it: "lexing error".to_string(),
                    at: TextRange::new(
                        TextSize::from(tkn.span.start as u32),
                        TextSize::from(tkn.span.end as u32),
                    ),
                })
            }
            self.builder
                .token(NemoLanguage::kind_to_raw(tkn.kind), tkn.text);
        }
    }

    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind))
    }

    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.current() != kind {
            return false;
        }
        self.bump_any();
        true
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            true
        } else {
            // TODO: Avoid extra allocation
            self.error(&format!("expected {:?}", kind));
            false
        }
    }

    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        let len = self.tokens.len();
        if n >= len {
            SyntaxKind::EOF
        } else {
            self.tokens[len - n - 1].token.kind
        }
    }

    pub(crate) fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.nth(n) == kind
    }

    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    pub(crate) fn span(&self) -> TextRange {
        let span = self
            .tokens
            .last()
            .map(|t| t.token.span.clone())
            .unwrap_or_default();
        TextRange::new(
            TextSize::from(span.start as u32),
            TextSize::from(span.end as u32),
        )
    }

    pub(crate) fn error(&mut self, msg: &str) {
        self.errors.push(ParseError {
            it: msg.to_string(),
            at: self.span(),
        })
    }
}
