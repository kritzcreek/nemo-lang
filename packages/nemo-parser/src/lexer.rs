use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<'a> {
    pub leading: Vec<(SyntaxKind, &'a str)>,
    pub token: (SyntaxKind, &'a str),
    pub trailing: Vec<(SyntaxKind, &'a str)>,
}

pub(crate) struct Lexer<'a> {
    inner: logos::Lexer<'a, SyntaxKind>,
    lookahead: Option<(SyntaxKind, &'a str)>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            inner: SyntaxKind::lexer(input),
            lookahead: None,
        }
    }

    pub(crate) fn lex(input: &'a str) -> Vec<Token<'a>> {
        let mut lexer = Lexer::new(input);
        let mut tokens = vec![];
        loop {
            let tkn = lexer.next().unwrap();
            if tkn.token.0 == SyntaxKind::EOF {
                tokens.push(tkn);
                return tokens;
            }
            tokens.push(tkn);
        }
    }
}

fn is_trailing(kind: SyntaxKind) -> bool {
    kind == SyntaxKind::SPACE
}

fn is_whitespace(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SPACE | SyntaxKind::LINEFEED | SyntaxKind::LINE_COMMENT
    )
}

impl<'a> Lexer<'a> {
    fn inner_peek(&mut self) -> Option<(SyntaxKind, &'a str)> {
        match self.lookahead {
            Some(i) => Some(i),
            None => {
                // TODO: Propagate errors
                self.lookahead = Some((self.inner.next()?.ok()?, self.inner.slice()));
                self.lookahead
            }
        }
    }
    fn inner_next(&mut self) -> Option<(SyntaxKind, &'a str)> {
        match self.lookahead {
            Some(i) => {
                self.lookahead = None;
                Some(i)
            }
            // TODO: Propagate errors
            None => Some((self.inner.next()?.ok()?, self.inner.slice())),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut leading = vec![];
        let mut trailing = vec![];
        let mut next_token = (SyntaxKind::EOF, "");

        while matches!(self.inner_peek(), Some(tkn) if is_whitespace(tkn.0)) {
            leading.push(self.inner_next().unwrap())
        }

        if let Some(tkn) = self.inner_next() {
            next_token = tkn;
        }

        while matches!(self.inner_peek(), Some(tkn) if is_trailing(tkn.0)) {
            trailing.push(self.inner_next().unwrap())
        }

        Some(Token {
            leading,
            token: next_token,
            trailing,
        })
    }
}

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Logos, FromPrimitive, ToPrimitive,
)]
#[repr(u16)]
#[allow(bad_style, missing_docs, unreachable_pub)]
pub enum SyntaxKind {
    #[regex(" +")]
    SPACE,

    #[regex(r"//[^\r\n]*")]
    LINE_COMMENT,

    #[regex("\n|\r\n")]
    LINEFEED,

    #[token("fn")]
    FUNC_KW,

    #[token("let")]
    LET_KW,

    #[token("true")]
    TRUE_KW,

    #[token("false")]
    FALSE_KW,

    #[regex("[A-Za-z][A-Za-z0-9]*")]
    IDENT,

    #[regex("[0-9]+")]
    NUMBER_LIT,

    #[token(".")]
    DOT,

    #[token("+")]
    PLUS,

    #[token("-")]
    MINUS,

    #[token("_")]
    UNDERSCORE,

    #[token("*")]
    STAR,

    #[token("/")]
    SLASH,

    #[token("=")]
    EQUALS,

    #[token("==")]
    DOUBLE_EQUALS,

    #[token("(")]
    L_PAREN,

    #[token(")")]
    R_PAREN,

    #[token("[")]
    L_BRACKET,

    #[token("]")]
    R_BRACKET,

    #[token("{")]
    L_BRACE,

    #[token("}")]
    R_BRACE,

    #[token("<")]
    L_ANGLE,

    #[token(">")]
    R_ANGLE,

    #[token(":")]
    COLON,

    #[token(";")]
    SEMICOLON,

    #[token(",")]
    COMMA,

    #[token("->")]
    ARROW,

    EOF,

    // Composite nodes

    // Types

    // Top level
    TopLet,

    // Expressions

    // Declarations

    // Root
    Root,

    // Patterns

    // Literals
    LITERAL,
    // Modifiers
}

#[macro_export]
macro_rules ! T {
    [;] => { SyntaxKind::SEMICOLON };
    [,] => { SyntaxKind::COMMA };
    ['('] => { SyntaxKind::L_PAREN };
    [')'] => { SyntaxKind::R_PAREN };
    ['{'] => { SyntaxKind::L_BRACE };
    ['}'] => { SyntaxKind::R_BRACE };
    ['['] => { SyntaxKind::L_BRACKET };
    [']'] => { SyntaxKind::R_BRACKET };
    [<] => { SyntaxKind::L_ANGLE };
    [>] => { SyntaxKind::R_ANGLE };
    [+] => { SyntaxKind::PLUS };
    [*] => { SyntaxKind::STAR };
    [/] => { SyntaxKind::SLASH };
    [_] => { SyntaxKind::UNDERSCORE };
    [.] => { SyntaxKind::DOT };
    [:] => { SyntaxKind::COLON };
    [=] => { SyntaxKind::EQUALS };
    [==] => { SyntaxKind::DOUBLE_EQUALS };
    [-] => { SyntaxKind::MINUS };
    [->] => { SyntaxKind::ARROW };
    [true] => { SyntaxKind::TRUE_KW };
    [false] => { SyntaxKind::FALSE_KW };
    [fn] => { SyntaxKind::FUNC_KW };
    [if] => { SyntaxKind::IF_KW };
    [else] => { SyntaxKind::ELSE_KW };
    [let] => { SyntaxKind::LET_KW };
    [struct] => { SyntaxKind::STRUCT_KW };
    [while] => { SyntaxKind::WHILE_KW };
    [number_lit] => { SyntaxKind::NUMBER_LIT };
    [ident] => { SyntaxKind::IDENT };
}
