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

impl<'a> Lexer<'a> {
    fn inner_peek(&mut self) -> Option<Token<'a>> {
        match &self.lookahead {
            Some(i) => Some(i.clone()),
            None => {
                let next_token = Some(Token {
                    kind: self.inner.next()?.unwrap_or(SyntaxKind::LEX_ERROR),
                    text: self.inner.slice(),
                    span: self.inner.span(),
                });
                self.lookahead.clone_from(&next_token);
                next_token
            }
        }
    }

    fn inner_next(&mut self) -> Option<Token<'a>> {
        self.lookahead.take().or_else(|| {
            Some(Token {
                kind: self.inner.next()?.unwrap_or(SyntaxKind::LEX_ERROR),
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

        while matches!(self.inner_peek(), Some(tkn) if tkn.kind.is_whitespace()) {
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

    #[token("global")]
    GLOBAL_KW,

    #[token("import")]
    IMPORT_KW,

    #[token("from")]
    FROM_KW,

    #[token("let")]
    LET_KW,

    #[token("set")]
    SET_KW,

    #[token("if")]
    IF_KW,

    #[token("else")]
    ELSE_KW,

    #[token("match")]
    MATCH_KW,

    #[token("struct")]
    STRUCT_KW,

    #[token("variant")]
    VARIANT_KW,

    #[token("while")]
    WHILE_KW,

    #[token("true")]
    TRUE_KW,

    #[token("false")]
    FALSE_KW,

    #[token("i32")]
    I32_BUILTIN,

    #[token("f32")]
    F32_BUILTIN,

    #[token("bool")]
    BOOL_BUILTIN,

    #[token("unit")]
    UNIT_BUILTIN,

    #[regex("[a-z_][A-Za-z0-9_]*")]
    IDENT,

    #[regex("[A-Z][A-Za-z0-9_]*")]
    UPPER_IDENT,

    #[regex(r"[0-9]+\.[0-9]+")]
    FLOAT_LIT,

    #[regex("[0-9]+")]
    INT_LIT,

    #[regex("0b[0-1]+")]
    BINARY_LIT,

    #[regex("0x[0-9a-fA-f]+")]
    HEX_LIT,

    #[token(".")]
    DOT,

    #[token("+")]
    PLUS,

    #[token("-")]
    MINUS,

    #[token("*")]
    STAR,

    #[token("/")]
    SLASH,

    #[token("=")]
    EQUALS,

    #[token("&&")]
    ANDAND,

    #[token("||")]
    OROR,

    #[token("==")]
    DOUBLE_EQUALS,

    #[token("!=")]
    NOT_EQUALS,

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

    #[token("|")]
    PIPE,

    #[token("#")]
    HASH,

    #[token("<")]
    L_ANGLE,

    #[token(">")]
    R_ANGLE,

    #[token(">=")]
    L_ANGLE_EQ,

    #[token("<=")]
    R_ANGLE_EQ,

    #[token("::")]
    DOUBLE_COLON,

    #[token(":")]
    COLON,

    #[token(";")]
    SEMICOLON,

    #[token(",")]
    COMMA,

    #[token("->")]
    ARROW,

    #[token("=>")]
    FAT_ARROW,

    LEX_ERROR,
    EOF,

    // Composite nodes
    Param,
    ParamTy,
    BinOp,
    TyArgList,
    EArgList,
    ETyArgList,
    Qualifier,

    // Types
    TyInt,
    TyFloat,
    TyBool,
    TyUnit,
    TyArray,
    TyFn,
    TyCons,
    TyVar,

    // Top level
    TopGlobal,
    TopFn,
    TopImport,
    TopStruct,
    TopVariant,

    // Imports
    ImpInternal,
    ImpExternal,

    // Structs
    StructField,
    EStructField,

    // Literals
    LitBool,
    LitInt,
    LitFloat,

    // Expressions
    ELit,
    EVar,
    EArray,
    EStruct,
    ECall,
    EParen,
    EBinary,
    EArrayIdx,
    EStructIdx,
    EIf,
    EMatch,
    EBlock,
    EIntrinsic,

    // Match expressions
    EMatchBranch,

    // Declarations
    DLet,
    DSet,
    DWhile,
    DExpr,

    // Set targets
    SetTarget,
    SetStruct,
    SetArray,

    // Root
    Root,

    // Patterns
    PatVariant,
    PatVar,

    // Modifiers

    // Recovery node
    Error,
}

impl SyntaxKind {
    pub fn is_whitespace(&self) -> bool {
        matches!(
            self,
            SyntaxKind::SPACE
                | SyntaxKind::LINEFEED
                | SyntaxKind::LINE_COMMENT
                | SyntaxKind::LEX_ERROR
        )
    }
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
    [|] => { SyntaxKind::PIPE };
    [#] => { SyntaxKind::HASH };
    [<] => { SyntaxKind::L_ANGLE };
    [>] => { SyntaxKind::R_ANGLE };
    [<=] => { SyntaxKind::L_ANGLE_EQ };
    [>=] => { SyntaxKind::R_ANGLE_EQ };
    [+] => { SyntaxKind::PLUS };
    [-] => { SyntaxKind::MINUS };
    [*] => { SyntaxKind::STAR };
    [/] => { SyntaxKind::SLASH };
    [&&] => { SyntaxKind::ANDAND };
    [||] => { SyntaxKind::OROR };
    [==] => { SyntaxKind::DOUBLE_EQUALS };
    [!=] => { SyntaxKind::NOT_EQUALS };
    [.] => { SyntaxKind::DOT };
    [::] => { SyntaxKind::DOUBLE_COLON };
    [:] => { SyntaxKind::COLON };
    [=] => { SyntaxKind::EQUALS };
    [->] => { SyntaxKind::ARROW };
    [=>] => { SyntaxKind::FAT_ARROW };
    [true] => { SyntaxKind::TRUE_KW };
    [false] => { SyntaxKind::FALSE_KW };
    [i32] => { SyntaxKind::I32_BUILTIN };
    [f32] => { SyntaxKind::F32_BUILTIN };
    [bool] => { SyntaxKind::BOOL_BUILTIN };
    [unit] => { SyntaxKind::UNIT_BUILTIN };
    [fn] => { SyntaxKind::FN_KW };
    [global] => { SyntaxKind::GLOBAL_KW };
    [import] => { SyntaxKind::IMPORT_KW };
    [from] => { SyntaxKind::FROM_KW };
    [if] => { SyntaxKind::IF_KW };
    [else] => { SyntaxKind::ELSE_KW };
    [match] => { SyntaxKind::MATCH_KW };
    [let] => { SyntaxKind::LET_KW };
    [set] => { SyntaxKind::SET_KW };
    [struct] => { SyntaxKind::STRUCT_KW };
    [variant] => { SyntaxKind::VARIANT_KW };
    [while] => { SyntaxKind::WHILE_KW };
    [import] => { SyntaxKind::IMPORT_KW };
    [int_lit] => { SyntaxKind::INT_LIT };
    [binary_lit] => { SyntaxKind::BINARY_LIT };
    [hex_lit] => { SyntaxKind::HEX_LIT };
    [float_lit] => { SyntaxKind::FLOAT_LIT };
    [ident] => { SyntaxKind::IDENT };
    [upper_ident] => { SyntaxKind::UPPER_IDENT };
}
