pub mod check;
pub mod errors;
mod ir;
pub mod names;

use self::{
    check::{Occurrence, Typechecker},
    errors::TyError,
};
use crate::syntax::{nodes::Root, token_ptr::SyntaxTokenPtr, SyntaxNodePtr};
use backend::ir::Name;
pub use backend::ir::{FuncTy, NameMap, Program, Ty};
use std::collections::HashMap;

#[derive(Debug)]
pub struct CheckResult<E> {
    pub errors: Vec<E>,
    pub names: HashMap<SyntaxTokenPtr, Occurrence<Name>>,
    pub typed_nodes: HashMap<SyntaxNodePtr, Ty>,
    pub name_map: NameMap,
    pub ir: Option<Program>,
}

pub fn check_prog(prog: Root) -> CheckResult<TyError> {
    let mut checker = Typechecker::new();
    let ir = checker.infer_program(prog);
    let name_map = checker.name_supply.name_map;
    CheckResult {
        errors: checker.errors,
        names: checker.names,
        name_map,
        typed_nodes: checker.typed_nodes,
        ir,
    }
}
