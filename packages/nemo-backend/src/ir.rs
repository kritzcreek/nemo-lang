use core::fmt;

use nemo_frontend::syntax;

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

pub type Ty = syntax::Ty;

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    pub internal: Name,
    pub ty: Ty,
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
pub enum Expr {}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub imports: Vec<Import>,
    pub types: Vec<Type>,
    pub globals: Vec<Global>,
    pub funcs: Vec<Func>,
}
