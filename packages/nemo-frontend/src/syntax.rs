use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

impl Pos {
    pub const SYN: Pos = Pos { line: 0, col: 0 };
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub const SYN: Span = Span {
        start: Pos::SYN,
        end: Pos::SYN,
    };
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T> {
    pub it: T,
    pub at: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

impl FromStr for Op {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Op::Add),
            "-" => Ok(Op::Sub),
            "*" => Ok(Op::Mul),
            "/" => Ok(Op::Div),
            "<" => Ok(Op::Lt),
            "<=" => Ok(Op::Le),
            ">" => Ok(Op::Gt),
            ">=" => Ok(Op::Ge),
            "==" => Ok(Op::Eq),
            "!=" => Ok(Op::Ne),
            "&&" => Ok(Op::And),
            "||" => Ok(Op::Or),
            _ => Err(format!("Unknown operator {s}")),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ty {
    I32,
    F32,
    Unit,
    Bool,
    Array(Box<Ty>),
    Struct(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncTy {
    pub arguments: Vec<Ty>,
    pub result: Ty,
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    I32(i32),
    F32(f32),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub struct Typed<T> {
    pub ty: Ty,
    pub at: Span,
    pub it: T,
}

pub type TypedExpr = Typed<Expr>;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Var(String),
    Call {
        func: Spanned<String>,
        func_ty: FuncTy,
        arguments: Vec<TypedExpr>,
    },
    Binary {
        op: Spanned<Op>,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Toplevel {
    TopLet {
        binder: Typed<String>,
        expr: TypedExpr,
    },
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub toplevels: Vec<Toplevel>,
}
