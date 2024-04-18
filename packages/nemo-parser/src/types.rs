pub mod check;
pub mod errors;
pub mod names;

use std::{collections::HashMap, fmt};

use crate::syntax::{nodes::Root, token_ptr::SyntaxTokenPtr, SyntaxNodePtr};

use self::{
    check::{Occurence, Typechecker},
    errors::TyError,
};

pub use nemo_backend::ir::{FuncTy, Ty};

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
