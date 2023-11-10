use core::fmt;

use nemo_frontend::syntax;

pub type Op = syntax::Op;
pub type Lit = syntax::Lit;

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

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    pub internal: Name,
    pub ty: FuncTy,
    pub external: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    pub name: Name,
    pub fields: Vec<(Name, Ty)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Global {
    pub name: Name,
    pub ty: Ty,
    pub init: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    pub name: Name,
    pub params: Vec<(Name, Ty)>,
    pub return_ty: Ty,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit {
        ty: Ty,
        lit: Lit,
    },
    Var {
        ty: Ty,
        name: Name,
    },
    Call {
        ty: Ty,
        func: Name,
        arguments: Vec<Expr>,
    },
    Binary {
        ty: Ty,
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Array {
        ty: Ty,
        elements: Vec<Expr>,
    },
    ArrayIdx {
        ty: Ty,
        array: Box<Expr>,
        index: Box<Expr>,
    },
    If {
        ty: Ty,
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Block {
        ty: Ty,
        declarations: Vec<Declaration>,
    },
    Struct {
        ty: Ty,
        name: Name,
        fields: Vec<(Name, Expr)>,
    },
    StructIdx {
        ty: Ty,
        expr: Box<Expr>,
        index: Name,
    },
    Intrinsic {
        ty: Ty,
        intrinsic: Name,
        arguments: Vec<Expr>,
    },
}

impl Expr {
    pub fn ty(&self) -> &Ty {
        match self {
            Expr::Lit { ref ty, .. } => ty,
            Expr::Var { ref ty, .. } => ty,
            Expr::Call { ref ty, .. } => ty,
            Expr::Binary { ref ty, .. } => ty,
            Expr::Array { ref ty, .. } => ty,
            Expr::ArrayIdx { ref ty, .. } => ty,
            Expr::If { ref ty, .. } => ty,
            Expr::Block { ref ty, .. } => ty,
            Expr::Struct { ref ty, .. } => ty,
            Expr::StructIdx { ref ty, .. } => ty,
            Expr::Intrinsic { ref ty, .. } => ty,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SetTarget {
    Array { name: Name, index: Expr },
    Struct { name: Name, index: Name },
    Var { name: Name },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Let { binder: Name, expr: Expr },
    Set { set_target: SetTarget, expr: Expr },
    Expr(Expr),
    While { condition: Expr, body: Expr },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub imports: Vec<Import>,
    pub types: Vec<Type>,
    pub globals: Vec<Global>,
    pub funcs: Vec<Func>,
}
