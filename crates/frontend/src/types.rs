mod check;
mod error;
mod module;
mod names;

use crate::ir::{Ctx, ModuleId, MutableNameSupply, Name, Program};
use crate::syntax::token_ptr::SyntaxTokenPtr;
use crate::syntax::Module;
use check::Typechecker;
use std::collections::HashMap;

pub use check::{Occurrence, OccurrenceMap};
pub use error::TyError;
pub use module::{FuncDef, Interface, StructDef, StructFields, TypeDef, VariantDef};

#[derive(Debug)]
pub struct CheckResult<N, E> {
    pub errors: Vec<E>,
    pub occurrences: HashMap<SyntaxTokenPtr, Occurrence<Name>>,
    pub names: N,
    pub interface: Interface,
    pub ir: Option<Program>,
    pub parse: Module,
}

pub fn check_module(
    ctx: &Ctx,
    module: Module,
    module_id: ModuleId,
    dependencies: Vec<(String, Interface)>,
) -> CheckResult<MutableNameSupply, TyError> {
    let mut checker = Typechecker::new(module_id, dependencies);
    let (ir, interface, errors) = checker.infer_module(&module);
    let (names, _) = checker.name_supply.take();
    CheckResult {
        errors,
        occurrences: checker.occurrences,
        names,
        ir,
        interface,
        parse: module,
    }
}
