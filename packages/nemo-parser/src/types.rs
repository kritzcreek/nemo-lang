pub mod check;
pub mod errors;
mod ir;
pub mod names;

use self::{
    check::{Occurence, Typechecker},
    errors::TyError,
};
use crate::syntax::{nodes::Root, token_ptr::SyntaxTokenPtr, SyntaxNodePtr};
pub use nemo_backend::ir::{FuncTy, NameMap, Program, Ty};
use std::collections::HashMap;

pub struct CheckResult {
    pub errors: Vec<TyError>,
    pub names: HashMap<SyntaxTokenPtr, Occurence<String>>,
    pub typed_nodes: HashMap<SyntaxNodePtr, Ty>,
    pub name_map: NameMap,
    pub ir: Option<Program>,
}

pub fn check_prog(prog: Root) -> CheckResult {
    let mut checker = Typechecker::new();
    let ir = checker.infer_program(prog);
    let mut names = HashMap::new();
    for (k, v) in checker.names {
        let occ = match v {
            Occurence::Def(n) => Occurence::Def(checker.name_supply.lookup(n).unwrap().it.clone()),
            Occurence::Ref(n) => Occurence::Ref(checker.name_supply.lookup(n).unwrap().it.clone()),
        };
        names.insert(k, occ);
    }
    let name_map = checker.name_supply.name_map;
    CheckResult {
        errors: checker.errors,
        names,
        name_map,
        typed_nodes: checker.typed_nodes,
        ir,
    }
}
