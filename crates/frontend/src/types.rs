mod check;
mod error;
mod names;

use crate::ir::{Name, NameSupply, Program};
use crate::syntax::{token_ptr::SyntaxTokenPtr, Root};
use check::Typechecker;
use std::collections::HashMap;

pub use check::{Occurrence, OccurrenceMap};
pub use error::TyError;

#[derive(Debug)]
pub struct CheckResult<E> {
    pub errors: Vec<E>,
    pub occurrences: HashMap<SyntaxTokenPtr, Occurrence<Name>>,
    pub names: NameSupply,
    pub ir: Option<Program>,
    pub parse: Root,
}

pub fn check_prog(prog: Root) -> CheckResult<TyError> {
    let mut checker = Typechecker::new();
    let (ir, errors) = checker.infer_program(&prog);
    CheckResult {
        errors,
        occurrences: checker.occurrences,
        names: checker.name_supply.take(),
        ir,
        parse: prog,
    }
}
