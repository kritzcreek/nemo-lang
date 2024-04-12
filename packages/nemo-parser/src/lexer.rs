use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};
use std::ops::Range;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Token<'a> {
    pub kind: SyntaxKind,
    pub text: &'a str,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TToken<'a> {
    pub leading: Vec<Token<'a>>,
    pub token: Token<'a>,
    pub trailing: Vec<Token<'a>>,
}

pub(crate) struct Lexer<'a> {
    inner: logos::Lexer<'a, SyntaxKind>,
    lookahead: Option<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            inner: SyntaxKind::lexer(input),
            lookahead: None,
        }
    }

    pub(crate) fn lex(input: &'a str) -> Vec<TToken<'a>> {
        let mut lexer = Lexer::new(input);
        let mut tokens = vec![];
        loop {
            let tkn = lexer.next().unwrap();
            if tkn.token.kind == SyntaxKind::EOF {
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
    fn inner_peek(&mut self) -> Option<Token<'a>> {
        match &self.lookahead {
            Some(i) => Some(i.clone()),
            None => {
                let next_token = Some(Token {
                    kind: self.inner.next()?.ok()?,
                    text: self.inner.slice(),
                    span: self.inner.span(),
                });
                self.lookahead = next_token.clone();
                next_token
            }
        }
    }

    fn inner_next(&mut self) -> Option<Token<'a>> {
        self.lookahead.take().or_else(|| {
            Some(Token {
                kind: self.inner.next()?.ok()?,
                text: self.inner.slice(),
                span: self.inner.span(),
            })
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = TToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut leading = vec![];
        let mut trailing = vec![];
        let mut next_token = Token {
            kind: SyntaxKind::EOF,
            text: "",
            span: self.inner.span(),
        };

        while matches!(self.inner_peek(), Some(tkn) if is_whitespace(tkn.kind)) {
            leading.push(self.inner_next().unwrap())
        }

        if let Some(tkn) = self.inner_next() {
            next_token = tkn;
        }

        while matches!(self.inner_peek(), Some(tkn) if is_trailing(tkn.kind)) {
            trailing.push(self.inner_next().unwrap())
        }

        Some(TToken {
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
    FN_KW,

    #[token("let")]
    LET_KW,

    #[token("true")]
    TRUE_KW,

    #[token("false")]
    FALSE_KW,

    #[token("i32")]
    I32_BUILTIN,

    #[token("f32")]
    F32_BUILTIN,

    #[regex("[A-Za-z][A-Za-z0-9]*")]
    IDENT,

    #[regex("[0-9]+")]
    INT_LIT,

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
    ParamList,
    Param,

    // Types
    TyI32,
    TyF32,

    // Top level
    TopLet,
    TopFn,

    // Literals
    LitBool,
    LitInt,

    // Expressions
    ELit,

    // Declarations

    // Root
    Root,

    // Patterns

    // Literals
    LITERAL,
    // Modifiers

    // Recovery node
    Error,
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
    [i32] => { SyntaxKind::I32_BUILTIN };
    [f32] => { SyntaxKind::F32_BUILTIN };
    [fn] => { SyntaxKind::FN_KW };
    [if] => { SyntaxKind::IF_KW };
    [else] => { SyntaxKind::ELSE_KW };
    [let] => { SyntaxKind::LET_KW };
    [struct] => { SyntaxKind::STRUCT_KW };
    [while] => { SyntaxKind::WHILE_KW };
    [int_lit] => { SyntaxKind::INT_LIT };
    [ident] => { SyntaxKind::IDENT };
}
