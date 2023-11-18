use core::fmt;
use nemo_frontend::syntax::{Span, Spanned};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Name {
    Global(u32),
    Local(u32),
    Func(u32),
    Type(u32),
    Field(u32),
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Global(x) => write!(f, "$g:{x}"),
            Name::Local(x) => write!(f, "$l:{x}"),
            Name::Func(x) => write!(f, "$fn:{x}"),
            Name::Type(x) => write!(f, "$t:{x}"),
            Name::Field(x) => write!(f, "$f:{x}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Ty {
    I32,
    F32,
    Unit,
    Bool,
    Array(Box<Ty>),
    Struct(Name),
    Func(Box<FuncTy>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
// 4. The only desugarings we do at this point in time:
//    a) Transform set_targets to expr + index pairs, which which makes it easier to generate
//       code for these eventually
//    b) Transform all blocks into a list of declarations and a trailing expression, inserting
//       a dummy Unit expression if the last declaration is not a Declaration::Expr
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
pub enum Callee {
    FuncRef(Expr),
    Func(Name),
    Builtin(&'static str),
}

#[derive(Debug, PartialEq)]
pub enum ExprData {
    Lit(Lit),
    Var(Name),
    Call {
        func: Callee,
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
        // All IR blocks end in an expression, lowering inserts a dummy unit expression
        expr: Expr,
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

impl Func {
    pub fn func_ty(&self) -> FuncTy {
        FuncTy {
            arguments: self.params.iter().map(|(_, t)| t.clone()).collect(),
            result: self.return_ty.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub imports: Vec<Import>,
    pub structs: Vec<Struct>,
    pub globals: Vec<Global>,
    pub funcs: Vec<Func>,
    pub start_fn: Name,
}
