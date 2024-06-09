use crate::syntax::{AstNode, Root, SyntaxNode};
use rowan::GreenNode;
use std::str;

mod error;
mod grammar;
mod lexer;
mod parser;

pub use error::{render_parse_error, ParseError};
pub use lexer::{SyntaxKind, TToken};

pub fn parse_prog(input: &str) -> Parse {
    let mut p = parser::Parser::new(input);
    let c = p.checkpoint();
    grammar::prog(&mut p);
    if !p.at(SyntaxKind::EOF) {
        p.error("failed to make progress");
        p.start_node(SyntaxKind::Error);
        while !p.at(SyntaxKind::EOF) {
            p.bump_any()
        }
        p.finish_node()
    }
    p.finish_at(c, SyntaxKind::Root);
    let green_node = p.builder.finish();
    Parse {
        green_node,
        errors: p.errors,
    }
}

pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
}

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn root(&self) -> Root {
        Root::cast(self.syntax()).unwrap()
    }

    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
        formatted[0..formatted.len() - 1].to_string()
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}
