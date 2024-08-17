pub mod builtins;
mod error;
pub mod highlight;
pub mod ir;
mod module_dag;
pub mod parser;
pub mod syntax;
pub mod types;

use ir::{Ctx, ModuleId, ModuleIdGen};
use parser::parse_prog;

pub use error::CheckError;
use syntax::Module;
pub use types::CheckResult;

#[derive(Debug)]
pub struct FrontendResult {
    pub ir: Option<ir::Program>,
    pub ctx: Ctx,
    pub errors: Vec<CheckError>,
}

/// Runs the full frontend on `source` and returns the generated IR and other structures.
/// If there are any errors, the generated IR should _not_ be used. It's returned here for
/// debugging purposes.
pub fn run_frontend(source: &str) -> FrontendResult {
    // TODO
    let (parse_root, parse_errors) = parse_prog(source).take();
    let mut errors = vec![];
    for error in parse_errors {
        errors.push(CheckError::ParseError(error));
    }
    // Special case for single module without module header
    let parsed_modules: Vec<Module> = parse_root.modules().collect();
    if parsed_modules.len() == 1 {
        let module_id = ModuleIdGen::new().next();
        let module = parsed_modules.into_iter().next().unwrap();
        let check_result = types::check_module(&Ctx::new(1), module, module_id);
        let mut ctx = Ctx::new(1);
        ctx.set_module_name(module_id, "main".to_owned());
        ctx.set_interface(module_id, check_result.interface.clone());
        ctx.set_name_supply(module_id, check_result.names);
        for error in check_result.errors {
            errors.push(CheckError::TypeError(error));
        }
        return FrontendResult {
            ir: check_result.ir,
            ctx,
            errors,
        };
    }

    let modules = module_dag::toposort_modules(parse_root.clone());
    let mut ctx = Ctx::new(modules.len() as u16);
    let mut ir = Some(ir::Program::default());
    for (id, name, module) in modules {
        let check_result = types::check_module(&ctx, module, id);
        ctx.set_module_name(id, name);
        ctx.set_interface(id, check_result.interface.clone());
        ctx.set_name_supply(id, check_result.names);
        for error in check_result.errors {
            errors.push(CheckError::TypeError(error));
        }
        if let Some(ref mut ir) = ir {
            if let Some(module_ir) = check_result.ir {
                ir.imports.extend(module_ir.imports);
                ir.globals.extend(module_ir.globals);
                ir.types.extend(module_ir.types);
                ir.funcs.extend(module_ir.funcs);
            }
        }
    }
    if errors.is_empty() && ir.is_none() {
        panic!("No IR generated, despite no errors")
    };

    FrontendResult { errors, ctx, ir }
}

/// Checks the given program, and prints any parse or type errors.
/// If there are any errors returns a summary message in Err otherwise Ok.
pub fn check_program(source: &str) -> Result<(), String> {
    let check_result = run_frontend(source);
    if !check_result.errors.is_empty() {
        for err in &check_result.errors {
            eprintln!("{}", err.display(source, &check_result.ctx, true));
        }
        return Err(format!(
            "Check failed with {} errors",
            check_result.errors.len()
        ));
    }
    Ok(())
}
