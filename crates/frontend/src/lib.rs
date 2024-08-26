pub mod builtins;
mod error;
pub mod highlight;
pub mod ir;
mod module_dag;
pub mod parser;
pub mod syntax;
pub mod types;

use camino::Utf8PathBuf;
pub use error::CheckError;
use ir::{Ctx, ModuleId, ModuleIdGen, Program};
use parser::{parse_prog, ParseError};
use std::collections::HashMap;
use syntax::Module;
pub use types::CheckResult;
use types::{OccurrenceMap, TyError};

#[derive(Debug)]
pub struct FrontendResult<'src> {
    pub ctx: Ctx,
    pub modules: Vec<ModuleResult<'src>>,
}

impl FrontendResult<'_> {
    pub fn errors(&self) -> impl Iterator<Item = CheckError> {
        self.modules.iter().flat_map(|module| {
            module.ty_errors.iter().map(CheckError::TypeError).chain(
                module
                    .parse_result
                    .parse_errors
                    .iter()
                    .map(CheckError::ParseError),
            )
        })
    }

    pub fn display_errors(&self) -> Option<i32> {
        let mut count = 0;
        for module in &self.modules {
            let path = module.parse_result.path.as_path();
            for error in &module.parse_result.parse_errors {
                count += 1;
                println!("{}", error.display(path, module.parse_result.source, true));
            }
            for error in &module.ty_errors {
                count += 1;
                println!(
                    "{}",
                    error.display(&self.ctx, path, module.parse_result.source, true)
                );
            }
        }
        if count > 0 {
            Some(count)
        } else {
            None
        }
    }

    pub fn consume(self) -> (Ctx, Option<ir::Program>) {
        let mut ir = ir::Program::default();
        for module in self.modules {
            if let Some(module_ir) = module.ir {
                ir.merge(module_ir)
            } else {
                return (self.ctx, None);
            }
        }
        (self.ctx, Some(ir))
    }
}

#[derive(Debug)]
pub struct ModuleResult<'src> {
    pub parse_result: ModuleParseResult<'src>,
    pub occurrences: OccurrenceMap,
    pub ty_errors: Vec<TyError>,
    pub ir: Option<Program>,
}

#[derive(Debug)]
pub struct ModuleParseResult<'src> {
    pub id: ModuleId,
    pub parse_errors: Vec<ParseError>,
    pub source: &'src str,
    pub parse: Module,
    pub path: Utf8PathBuf,
    pub name: String,
    pub dependencies: Vec<String>,
}

fn module_name(module: &Module) -> Option<String> {
    Some(module.mod_header()?.ident_token()?.text().to_string())
}

fn extract_module_header(module: &Module) -> (String, Vec<String>) {
    let name = module_name(module).unwrap_or_default();
    let dependencies = module
        .mod_uses()
        .filter_map(|mod_use| mod_use.ident_token().map(|t| t.text().to_string()))
        .collect();
    (name, dependencies)
}

/// Runs the full frontend on `sources` and returns the generated IR and other structures.
/// If there are any errors, the generated IR should _not_ be used. It's returned here for
/// debugging purposes.
pub fn run_frontend(sources: &[(Utf8PathBuf, String)]) -> FrontendResult {
    let mut id_gen = ModuleIdGen::new();
    let mut parsed_modules: HashMap<ModuleId, ModuleParseResult> = HashMap::new();
    for (path, source) in sources {
        let id = id_gen.next_id();
        let (parse_root, parse_errors) = parse_prog(source).take();
        let parse = parse_root
            .module()
            .expect("Failed to parse a module from file: {path}");
        let (name, dependencies) = extract_module_header(&parse);
        parsed_modules.insert(
            id,
            ModuleParseResult {
                id,
                source,
                name,
                dependencies,
                parse_errors,
                parse,
                path: path.clone(),
            },
        );
    }
    let module_sort_input: Vec<_> = parsed_modules
        .values()
        .map(|parsed_module| {
            (
                parsed_module.id,
                parsed_module.name.as_str(),
                parsed_module.dependencies.as_slice(),
            )
        })
        .collect();

    let sorted_modules = module_dag::toposort_modules(module_sort_input);
    let mut ctx = Ctx::new(sources.len() as u16);
    let mut checked_ids = vec![];
    let mut checked_modules = vec![];
    for id in sorted_modules {
        let parsed_module = parsed_modules.remove(&id).unwrap();
        let check_result = types::check_module(&ctx, parsed_module.parse.clone(), id, &checked_ids);
        ctx.set_module_name(id, parsed_module.name.clone());
        ctx.set_interface(id, check_result.interface);
        ctx.set_name_supply(id, check_result.names);
        if parsed_module.parse_errors.is_empty()
            && check_result.errors.is_empty()
            && check_result.ir.is_none()
        {
            panic!("No IR generated, despite no errors")
        };
        checked_ids.push(id);
        checked_modules.push(ModuleResult {
            parse_result: parsed_module,
            occurrences: check_result.occurrences,
            ty_errors: check_result.errors,
            ir: check_result.ir,
        });
    }

    FrontendResult {
        ctx,
        modules: checked_modules,
    }
}

/// Checks the given sources and prints any parse or type errors.
/// If there are any errors returns a summary message in Err otherwise Ok.
pub fn check_program(sources: &[(Utf8PathBuf, String)]) -> Result<(), String> {
    let check_result = run_frontend(sources);
    if let Some(count) = check_result.display_errors() {
        return Err(format!("Check failed with {} errors", count));
    }
    Ok(())
}
