mod check;
mod error;
mod names;

use crate::ir::{FuncTy, Program, Ty};
use crate::ir::{Name, NameSupply};
use crate::syntax::{token_ptr::SyntaxTokenPtr, Root, SyntaxNodePtr};
use check::Typechecker;
pub use check::{Occurrence, OccurrenceMap};
pub use error::TyError;
use std::collections::HashMap;

#[derive(Debug)]
pub struct CheckResult<E> {
    pub errors: Vec<E>,
    pub occurrences: HashMap<SyntaxTokenPtr, Occurrence<Name>>,
    pub typed_nodes: HashMap<SyntaxNodePtr, Ty>,
    pub names: NameSupply,
    pub ir: Option<Program>,
    pub parse: Root,
}

pub fn check_prog(prog: Root) -> CheckResult<TyError> {
    let mut checker = Typechecker::new();
    let (ir, errors) = checker.infer_program(&prog);
    CheckResult {
        errors,
        occurrences: checker.names,
        names: checker.name_supply.take(),
        typed_nodes: checker.typed_nodes,
        ir,
        parse: prog,
    }
}
