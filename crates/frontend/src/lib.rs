pub mod builtins;
mod error;
pub mod highlight;
pub mod ir;
mod module_dag;
pub mod parser;
pub mod syntax;
pub mod types;

use ir::{Ctx, ModuleId, ModuleIdGen, Program};
use parser::{parse_prog, ParseError};

pub use error::CheckError;
use syntax::Module;
pub use types::CheckResult;
use types::{OccurrenceMap, TyError};

// TODO: Customizable display for debugging
#[derive(Debug)]
pub struct FrontendModuleResult {
    pub id: ModuleId,
    // path/offset?
    pub ty_errors: Vec<TyError>,
    pub occurrences: OccurrenceMap,
    pub ir: Option<Program>,
    pub parse: Module,
}

#[derive(Debug)]
pub struct FrontendResult {
    pub ctx: Ctx,
    pub parse_errors: Vec<ParseError>,
    pub modules: Vec<FrontendModuleResult>,
}

impl FrontendResult {
    pub fn errors<'a>(&'a self) -> impl Iterator<Item = CheckError<'a>> {
        self.parse_errors.iter().map(CheckError::ParseError).chain(
            self.modules
                .iter()
                .flat_map(|module| module.ty_errors.iter().map(CheckError::TypeError)),
        )
    }

    pub fn has_errors(&self) -> bool {
        self.parse_errors.len() > 0
            || self
                .modules
                .iter()
                .any(|module| !module.ty_errors.is_empty())
    }

    pub fn display(&self, source: &str) {
        for module in &self.modules {
            for error in &module.ty_errors {
                eprintln!("{}", error.display(source, &self.ctx, true));
            }
        }
    }

    pub fn consume(self) -> (Ctx, Option<Program>) {
        let mut ir = Some(ir::Program::default());
        for module in self.modules {
            match (&mut ir, module.ir) {
                (Some(ir), Some(module_ir)) => {
                    ir.merge(module_ir);
                }
                (_, _) => {
                    ir = None;
                    break;
                }
            }
        }
        (self.ctx, ir)
    }
}

/// Runs the full frontend on `source` and returns the generated IR and other structures.
/// If there are any errors, the generated IR should _not_ be used. It's returned here for
/// debugging purposes.
pub fn run_frontend(source: &str) -> FrontendResult {
    // TODO multiple files
    let (parse_root, parse_errors) = parse_prog(source).take();

    // Special case for single module without module header
    let parsed_modules: Vec<Module> = parse_root.modules().collect();
    let modules = if parsed_modules.len() == 1 {
        vec![(
            ModuleIdGen::new().next(),
            "main".to_owned(),
            parsed_modules[0].clone(),
        )]
    } else {
        module_dag::toposort_modules(parse_root.clone())
    };
    let mut ctx = Ctx::new(modules.len() as u16);
    let mut checked_modules = vec![];
    for (id, name, module) in modules {
        let check_result = types::check_module(&ctx, module.clone(), id);
        ctx.set_module_name(id, name);
        ctx.set_interface(id, check_result.interface.clone());
        ctx.set_name_supply(id, check_result.names);
        if check_result.errors.is_empty() && check_result.ir.is_none() {
            panic!("No IR generated, despite no errors")
        };
        checked_modules.push(FrontendModuleResult {
            id,
            parse: module,
            ty_errors: check_result.errors,
            occurrences: check_result.occurrences,
            ir: check_result.ir,
        });
    }

    FrontendResult {
        ctx,
        parse_errors,
        modules: checked_modules,
    }
}

/// Checks the given program, and prints any parse or type errors.
/// If there are any errors returns a summary message in Err otherwise Ok.
pub fn check_program(source: &str) -> Result<(), String> {
    let check_result = run_frontend(source);
    if check_result.has_errors() {
        let mut count = 0;
        for err in check_result.errors() {
            count += 1;
            eprintln!("{}", err.display(source, &check_result.ctx, true));
        }
        return Err(format!("Check failed with {} errors", count));
    }
    Ok(())
}
