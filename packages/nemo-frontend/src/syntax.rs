use core::fmt;
use std::str::FromStr;

use tree_sitter::{Node, Point};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Pos {
    pub line: u32,
    pub column: u32,
}

impl Pos {
    pub const SYN: Pos = Pos { line: 0, column: 0 };
}

impl From<Point> for Pos {
    fn from(value: Point) -> Self {
        Pos {
            line: value.row as u32,
            column: value.column as u32,
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
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

impl From<Node<'_>> for Span {
    fn from(value: Node<'_>) -> Self {
        Span {
            start: value.start_position().into(),
            end: value.end_position().into(),
        }
    }
}

impl From<&Node<'_>> for Span {
    fn from(value: &Node<'_>) -> Self {
        Span {
            start: value.start_position().into(),
            end: value.end_position().into(),
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Spanned<T> {
    pub it: T,
    pub at: Span,
}

impl<T> Spanned<T> {
    pub fn from(at: Span, it: T) -> Spanned<T> {
        Spanned { it, at }
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.at, self.it)
    }
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

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::I32 => write!(f, "i32"),
            Ty::F32 => write!(f, "f32"),
            Ty::Bool => write!(f, "bool"),
            Ty::Unit => write!(f, "unit"),
            Ty::Array(t) => write!(f, "[{}]", t),
            Ty::Struct(t) => write!(f, "{}", t),
        }
    }
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
    Array(Vec<TypedExpr>),
    ArrayIdx {
        array: Box<TypedExpr>,
        index: Box<TypedExpr>,
    },
    If {
        condition: Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Box<TypedExpr>,
    },
}
#[derive(Debug, PartialEq, Clone)]
pub struct FuncParam {
    pub name: Spanned<String>,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub name: Spanned<String>,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialEq)]
pub enum Toplevel {
    TopLet {
        binder: Typed<String>,
        expr: TypedExpr,
    },
    TopImport {
        internal: Spanned<String>,
        func_ty: Spanned<FuncTy>,
        external: Spanned<String>,
    },
    TopStruct {
        name: Spanned<String>,
        fields: Vec<StructField>,
    },
    TopFunc {
        name: Spanned<String>,
        params: Vec<FuncParam>,
        return_ty: Option<Spanned<Ty>>,
        body: TypedExpr,
    },
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub toplevels: Vec<Toplevel>,
}
