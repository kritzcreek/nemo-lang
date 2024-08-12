pub mod builtins;
mod error;
pub mod highlight;
pub mod ir;
mod module_dag;
pub mod parser;
pub mod syntax;
pub mod types;

use ir::Ctx;
use parser::parse_prog;

pub use error::CheckError;
pub use types::CheckResult;

/// Runs the full frontend on `source` and returns the generated IR and other structures.
/// If there are any errors, the generated IR should _not_ be used. It's returned here for
/// debugging purposes.
pub fn run_frontend(source: &str) -> CheckResult<Ctx, CheckError> {
    // TODO
    let (parse_root, parse_errors) = parse_prog(source).take();
    let mut errors = vec![];
    for error in parse_errors {
        errors.push(CheckError::ParseError(error));
    }
    let modules = module_dag::toposort_modules(parse_root.clone());
    let mut ctx = Ctx::new(modules.len() as u16);
    for (id, name, module) in modules {
        let check_result = types::check_module(&ctx, module, id);
        dbg!(id, &name);
        ctx.set_module_name(id, name);
        ctx.set_interface(id, check_result.interface.clone());
        ctx.set_name_supply(id, check_result.names);
        for error in check_result.errors {
            errors.push(CheckError::TypeError(error));
        }
    }
    // dbg!(errors);
    // dbg!(ctx);
    return todo!();

    // if errors.is_empty() && check_result.ir.is_none() {
    // panic!("No IR generated, despite no errors")
    // };
    //
    // CheckResult {
    // errors,
    // names: ctx,
    // occurrences: check_result.occurrences,
    // interface: check_result.interface,
    // ir: check_result.ir,
    // parse: check_result.parse,
    // }
}

/// Checks the given program, and prints any parse or type errors.
/// If there are any errors returns a summary message in Err otherwise Ok.
pub fn check_program(source: &str) -> Result<(), String> {
    let check_result = run_frontend(source);
    if !check_result.errors.is_empty() {
        for err in &check_result.errors {
            eprintln!("{}", err.display(source, &check_result.names, true));
        }
        return Err(format!(
            "Check failed with {} errors",
            check_result.errors.len()
        ));
    }
    Ok(())
}
