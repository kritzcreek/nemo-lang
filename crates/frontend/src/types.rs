mod check;
mod error;
mod module;
mod names;

use crate::ir::{ModuleId, MutableNameSupply, Name, Program};
use crate::syntax::{token_ptr::SyntaxTokenPtr, Root};
use check::Typechecker;
use std::collections::HashMap;

pub use check::{Occurrence, OccurrenceMap};
pub use error::TyError;
pub use module::{Interface, Visibility};

#[derive(Debug)]
pub struct CheckResult<N, E> {
    pub errors: Vec<E>,
    pub occurrences: HashMap<SyntaxTokenPtr, Occurrence<Name>>,
    pub names: N,
    pub interface: Interface,
    pub ir: Option<Program>,
    pub parse: Root,
}

pub fn check_prog(prog: Root, module: ModuleId) -> CheckResult<MutableNameSupply, TyError> {
    let mut checker = Typechecker::new(module);
    let (ir, interface, errors) = checker.infer_program(&prog);
    let (names, _) = checker.name_supply.take();
    CheckResult {
        errors,
        occurrences: checker.occurrences,
        names,
        ir,
        interface,
        parse: prog,
    }
}
