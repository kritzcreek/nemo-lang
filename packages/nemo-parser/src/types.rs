pub mod check;
pub mod errors;
pub mod names;

use std::{collections::HashMap, fmt};

use crate::syntax::{nodes::Root, token_ptr::SyntaxTokenPtr, SyntaxNodePtr};

use self::{
    check::{Occurence, Typechecker},
    errors::TyError,
    names::Name,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ty {
    I32,
    F32,
    Unit,
    Bool,
    Array(Box<Ty>),
    Struct(Name),
    Func(Box<FuncTy>),

    // Typechecking internal used for error recovery
    Any,
}
impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::I32 => write!(f, "i32"),
            Ty::F32 => write!(f, "f32"),
            Ty::Bool => write!(f, "bool"),
            Ty::Unit => write!(f, "unit"),
            Ty::Array(t) => write!(f, "[{}]", t),
            Ty::Struct(t) => t.fmt(f),
            Ty::Func(func_ty) => func_ty.fmt(f),
            Ty::Any => write!(f, "ANY"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncTy {
    pub arguments: Vec<Ty>,
    pub result: Ty,
}

impl fmt::Display for FuncTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn ({}) -> {}",
            self.arguments
                .iter()
                .map(|a| format!("{a}"))
                .collect::<Vec<String>>()
                .join(", "),
            self.result
        )
    }
}

pub struct CheckResult {
    pub errors: Vec<TyError>,
    pub names: HashMap<SyntaxTokenPtr, Occurence<String>>,
    pub typed_nodes: HashMap<SyntaxNodePtr, Ty>,
}

pub fn check_prog(prog: Root) -> CheckResult {
    let mut checker = Typechecker::new();
    checker.infer_program(prog);
    let mut names = HashMap::new();
    for (k, v) in checker.names {
        let occ = match v {
            Occurence::Def(n) => Occurence::Def(checker.name_supply.lookup(n).unwrap().it.clone()),
            Occurence::Ref(n) => Occurence::Ref(checker.name_supply.lookup(n).unwrap().it.clone()),
        };
        names.insert(k, occ);
    }
    CheckResult {
        errors: checker.errors,
        names,
        typed_nodes: checker.typed_nodes,
    }
}
