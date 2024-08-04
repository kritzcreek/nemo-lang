mod check;
mod error;
mod module;
mod names;

use crate::ir::{ModuleId, Name, NameSupply, Program};
use crate::syntax::{token_ptr::SyntaxTokenPtr, Root};
use check::Typechecker;
use std::collections::HashMap;

pub use check::{Occurrence, OccurrenceMap};
pub use error::TyError;
pub use module::{Interface, Visibility};

#[derive(Debug)]
pub struct CheckResult<E> {
    pub errors: Vec<E>,
    pub occurrences: HashMap<SyntaxTokenPtr, Occurrence<Name>>,
    pub names: NameSupply,
    pub interface: Interface,
    pub ir: Option<Program>,
    pub parse: Root,
}

pub fn check_prog(prog: Root, module: ModuleId) -> CheckResult<TyError> {
    let mut checker = Typechecker::new(module);
    let (ir, interface, errors) = checker.infer_program(&prog);
    CheckResult {
        errors,
        occurrences: checker.occurrences,
        names: checker.name_supply.take(),
        ir,
        interface,
        parse: prog,
    }
}
