use crate::lexer::{Lexer, SyntaxKind, Token};
use crate::syntax::{NemoLanguage, SyntaxNode};
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, Language};

mod prog;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    builder: GreenNodeBuilder<'static>,
    // TODO: Be smarter here
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut tkns = Lexer::lex(input);
        tkns.reverse();
        Self {
            tokens: tkns,
            builder: GreenNodeBuilder::new(),
            errors: vec![],
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(NemoLanguage::kind_to_raw(kind));
    }

    fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder
            .start_node_at(checkpoint, NemoLanguage::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn finish_at(&mut self, c: Checkpoint, kind: SyntaxKind) {
        self.start_node_at(c, kind);
        self.finish_node();
    }

    fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
    }

    fn bump_any(&mut self) {
        let Token {
            leading,
            token: (kind, text),
            trailing,
        } = self.tokens.pop().unwrap();

        for (kind, text) in leading {
            self.builder
                .token(NemoLanguage::kind_to_raw(kind), text.into());
        }

        self.builder
            .token(NemoLanguage::kind_to_raw(kind), text.into());

        for (kind, text) in trailing {
            self.builder
                .token(NemoLanguage::kind_to_raw(kind), text.into());
        }
    }

    fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind))
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.current() != kind {
            return false;
        }
        self.bump_any();
        true
    }

    fn nth(&self, n: usize) -> SyntaxKind {
        let len = self.tokens.len();
        if n >= len {
            SyntaxKind::EOF
        } else {
            self.tokens[len - n - 1].token.0
        }
    }

    fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.nth(n) == kind
    }

    fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    fn error(&mut self, msg: &str) {
        self.errors.push(msg.to_string())
    }
}

pub fn parse_prog(input: &str) -> Parse {
    let mut p = Parser::new(input);
    prog::prog(&mut p);
    let green_node = p.builder.finish();
    Parse {
        green_node,
        errors: p.errors,
    }
}

pub struct Parse {
    green_node: GreenNode,
    errors: Vec<String>,
}

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
        formatted[0..formatted.len() - 1].to_string()
    }
}
