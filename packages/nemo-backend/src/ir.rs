use core::fmt;
use nemo_frontend::syntax::{Span, Spanned};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Name {
    Global(u32),
    Local(u32),
    Func(u32),
    Type(u32),
    Field(u32),
    // TODO
    Builtin(u32),
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Global(x) => write!(f, "$g:{x}"),
            Name::Local(x) => write!(f, "$l:{x}"),
            Name::Func(x) => write!(f, "$fn:{x}"),
            Name::Type(x) => write!(f, "$t:{x}"),
            Name::Field(x) => write!(f, "$f:{x}"),
            Name::Builtin(x) => write!(f, "$b:{x}"),
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
    Struct(Name),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncTy {
    pub arguments: Vec<Ty>,
    pub result: Ty,
}

// Our backend ast is very similar to our syntax ast.
//
// The main differences at this point in time are:
// 1. All names are replaced with unique numeric identifiers the `Name` type in this module
// 2. All syntactic type annotations are omitted, as we only care about the infered/semantic ones
// 3. All type-based resolution is done during lowering -> + becomes f32.+ or i32.+ and all struct indexes are
//    unique based on their type
// 4. The only desugaring we do at this point in time is to simplify set_targets to identifier + index pairs
//    eg: `set x.y.z = 1`` => `let $gen = x.y; set $gen.z = 1`
//
// We also try to keep as many Spans around as possible, mostly to help us when debugging our compiler

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
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32Lt,
    I32Gt,
    I32Le,
    I32Ge,
    I32Eq,
    I32Ne,

    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F32Eq,
    F32Ne,

    BoolEq,
    BoolNe,
    BoolAnd,
    BoolOr,
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
    Unit,
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
    Var(Name),
    Call {
        func: Name,
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
        name: Name,
        fields: Vec<(Name, Expr)>,
    },
    StructIdx {
        expr: Expr,
        index: Name,
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
    Let { binder: Name, expr: Expr },
    Set { set_target: SetTarget, expr: Expr },
    Expr(Expr),
    While { condition: Expr, body: Expr },
}

#[derive(Debug, PartialEq)]
pub struct SetTarget {
    pub it: SetTargetData,
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
    Array { target: Expr, index: Expr },
    Struct { target: Expr, index: Name },
    Var { name: Name },
}

#[derive(Debug, PartialEq)]
pub struct Import {
    pub span: Span,
    pub internal: Name,
    pub func_ty: FuncTy,
    pub external: String,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub span: Span,
    pub name: Name,
    pub fields: Vec<(Name, Ty)>,
}

#[derive(Debug, PartialEq)]
pub struct Global {
    pub span: Span,
    pub binder: Name,
    pub init: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub name: Name,
    pub params: Vec<(Name, Ty)>,
    pub return_ty: Ty,
    pub body: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub imports: Vec<Import>,
    pub structs: Vec<Struct>,
    pub globals: Vec<Global>,
    pub funcs: Vec<Func>,
}
