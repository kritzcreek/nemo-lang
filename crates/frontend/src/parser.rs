use crate::syntax::{AstNode, Root, SyntaxNode};
use rowan::GreenNode;
use std::str;

mod error;
mod grammar;
mod lexer;
mod parsing;

pub use error::ParseError;
pub use lexer::SyntaxKind;

pub fn parse_prog(input: &str) -> Parse {
    let mut p = parsing::Parser::new(input);
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
    green_node: GreenNode,
    errors: Vec<ParseError>,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
        formatted[0..formatted.len() - 1].to_string()
    }

    pub fn take(self) -> (Root, Vec<ParseError>) {
        (
            Root::cast(SyntaxNode::new_root(self.green_node)).unwrap(),
            self.errors,
        )
    }
}
