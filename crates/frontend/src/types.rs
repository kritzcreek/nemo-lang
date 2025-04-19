mod check;
mod error;
mod module;
mod names;

use crate::ir::{Ctx, ModuleId, Name, NameSupply, Program};
use crate::syntax::token_ptr::SyntaxTokenPtr;
use crate::syntax::Module;
use check::Typechecker;
use std::collections::HashMap;

pub use check::{Occurrence, OccurrenceMap};
pub use error::TyError;
pub use module::{FuncDef, Interface, StructDef, StructFields, TypeDef, VariantDef};

#[derive(Debug)]
pub struct CheckResult {
    pub errors: Vec<TyError>,
    pub occurrences: HashMap<SyntaxTokenPtr, Occurrence<Name>>,
    pub names: NameSupply,
    pub interface: Interface,
    pub ir: Program,
    pub parse: Module,
}

pub fn check_module(
    ctx: &Ctx,
    module: Module,
    module_id: ModuleId,
    checked_ids: &[ModuleId],
) -> CheckResult {
    let mut dependencies: Vec<(&str, &Interface)> = vec![];
    for id in checked_ids {
        dependencies.push((ctx.get_module_name(*id), ctx.get_interface(*id)));
    }
    let mut checker = Typechecker::new(module_id, &dependencies);
    let (ir, interface, errors) = checker.infer_module(&module);
    let (names, _) = checker.name_supply.take();
    CheckResult {
        errors,
        occurrences: checker.occurrences.take(),
        names,
        ir,
        interface,
        parse: module,
    }
}
