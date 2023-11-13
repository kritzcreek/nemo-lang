use core::fmt;
use std::str::FromStr;

use crate::types::{FuncTy, Ty};
use tree_sitter::{Node, Point};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Pos {
    pub line: u32,
    pub column: u32,
}

impl Pos {
    pub const SYN: Pos = Pos {
        line: u32::MAX,
        column: u32::MAX,
    };
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
    /// The synthetic Span for generated nodes
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

pub trait Spanned {
    fn at(&self) -> &Span;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type {
    pub it: Box<TypeData>,
    pub at: Span,
    pub ty: Ty,
}

impl Spanned for Type {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeData {
    I32,
    F32,
    Unit,
    Bool,
    Array(Type),
    Struct(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncType {
    pub it: FuncTypeData,
    pub at: Span,
    pub ty: FuncTy,
}

impl Spanned for FuncType {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncTypeData {
    pub arguments: Vec<Type>,
    pub result: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Id {
    pub it: String,
    pub at: Span,
}

impl Spanned for Id {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncId {
    pub it: String,
    pub at: Span,
    pub ty: FuncTy,
}

impl Spanned for FuncId {
    fn at(&self) -> &Span {
        &self.at
    }
}

impl FuncId {
    pub fn to_id(self) -> Id {
        Id {
            it: self.it,
            at: self.at,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Op {
    pub it: OpData,
    pub at: Span,
}

impl Spanned for Op {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum OpData {
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

impl FromStr for OpData {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(OpData::Add),
            "-" => Ok(OpData::Sub),
            "*" => Ok(OpData::Mul),
            "/" => Ok(OpData::Div),
            "<" => Ok(OpData::Lt),
            "<=" => Ok(OpData::Le),
            ">" => Ok(OpData::Gt),
            ">=" => Ok(OpData::Ge),
            "==" => Ok(OpData::Eq),
            "!=" => Ok(OpData::Ne),
            "&&" => Ok(OpData::And),
            "||" => Ok(OpData::Or),
            _ => Err(format!("Unknown operator {s}")),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lit {
    pub it: LitData,
    pub at: Span,
    pub ty: Ty,
}

impl Spanned for Lit {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LitData {
    I32(i32),
    F32(f32),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub struct Intrinsic {
    pub it: IntrinsicData,
    pub at: Span,
}

impl Spanned for Intrinsic {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IntrinsicData {
    ArrayLen,
    ArrayNew,
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub it: Box<ExprData>,
    pub at: Span,
    pub ty: Ty,
}

impl Spanned for Expr {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprData {
    Lit(Lit),
    Var(Id),
    Call {
        func: FuncId,
        arguments: Vec<Expr>,
    },
    Binary {
        op: Op,
        left: Expr,
        right: Expr,
    },
    Array(Vec<Expr>),
    ArrayIdx {
        array: Expr,
        index: Expr,
    },
    If {
        condition: Expr,
        then_branch: Expr,
        else_branch: Expr,
    },
    Block {
        declarations: Vec<Declaration>,
    },
    Struct {
        name: Id,
        fields: Vec<(Id, Expr)>,
    },
    StructIdx {
        expr: Expr,
        index: Id,
    },
    Intrinsic {
        intrinsic: Intrinsic,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub it: DeclarationData,
    pub at: Span,
    pub ty: Ty,
}

impl Spanned for Declaration {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq)]
pub enum DeclarationData {
    Let {
        binder: Id,
        annotation: Option<Type>,
        expr: Expr,
    },
    Set {
        set_target: SetTarget,
        expr: Expr,
    },
    Expr(Expr),
    While {
        condition: Expr,
        body: Expr,
    },
}

#[derive(Debug, PartialEq)]
pub struct SetTarget {
    pub it: Box<SetTargetData>,
    pub at: Span,
    pub ty: Ty,
}

impl Spanned for SetTarget {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq)]
pub enum SetTargetData {
    Array { target: SetTarget, index: Expr },
    Struct { target: SetTarget, index: Id },
    Var { name: Id },
}

#[derive(Debug, PartialEq)]
pub struct Toplevel {
    pub it: ToplevelData,
    pub at: Span,
}

impl Spanned for Toplevel {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug, PartialEq)]
pub enum ToplevelData {
    Import {
        internal: FuncId,
        func_ty: FuncType,
        external: Id,
    },
    Struct {
        name: Id,
        fields: Vec<(Id, Type)>,
    },
    Global {
        binder: Id,
        annotation: Option<Type>,
        init: Expr,
    },
    Func {
        name: FuncId,
        params: Vec<(Id, Type)>,
        return_ty: Option<Type>,
        body: Expr,
    },
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub toplevels: Vec<Toplevel>,
}
