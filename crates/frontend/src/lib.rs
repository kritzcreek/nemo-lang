pub mod builtins;
mod error;
pub mod highlight;
pub mod ir;
mod module_dag;
pub mod parser;
pub mod syntax;
pub mod types;

use std::collections::HashMap;

use camino::{Utf8Path, Utf8PathBuf};
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
    pub fn errors(&self) -> impl Iterator<Item = CheckError> {
        self.parse_errors.iter().map(CheckError::ParseError).chain(
            self.modules
                .iter()
                .flat_map(|module| module.ty_errors.iter().map(CheckError::TypeError)),
        )
    }

    pub fn has_errors(&self) -> bool {
        !self.parse_errors.is_empty()
            || self
                .modules
                .iter()
                .any(|module| !module.ty_errors.is_empty())
    }

    pub fn display(&self, source: &str) {
        for module in &self.modules {
            for error in &module.ty_errors {
                eprintln!(
                    "{}",
                    error.display(&self.ctx, Utf8Path::new(""), source, true)
                );
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

#[derive(Debug)]
pub struct FrontendResultNew<'src> {
    pub ctx: Ctx,
    pub modules: Vec<ModuleResult<'src>>,
}

impl FrontendResultNew<'_> {
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

        return (self.ctx, Some(ir));
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

pub fn run_frontend_new(sources: &[(Utf8PathBuf, String)]) -> FrontendResultNew {
    let mut id_gen = ModuleIdGen::new();
    let mut parsed_modules: HashMap<ModuleId, ModuleParseResult> = HashMap::new();
    for (path, source) in sources {
        let id = id_gen.next_id();
        let (parse_root, parse_errors) = parse_prog(source).take();
        let parse = parse_root
            .modules()
            .next()
            .expect("Failed to parse a module from file: {path}");
        let module_header = parse.mod_header().expect("Expected a module header");
        let name = module_header
            .ident_token()
            .expect("Expected a module name")
            .text()
            .to_string();
        let dependencies = module_header
            .mod_uses()
            .filter_map(|mod_use| mod_use.ident_token().map(|t| t.text().to_string()))
            .collect();
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
    let sorted_modules = module_dag::toposort_modules_new(module_sort_input);

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

    FrontendResultNew {
        ctx,
        modules: checked_modules,
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
            ModuleIdGen::new().next_id(),
            "main".to_owned(),
            parsed_modules[0].clone(),
        )]
    } else {
        module_dag::toposort_modules(parse_root.clone())
    };
    let mut ctx = Ctx::new(modules.len() as u16);
    let mut checked_modules = vec![];
    let mut checked_ids = vec![];
    for (id, name, module) in modules {
        let check_result = types::check_module(&ctx, module.clone(), id, &checked_ids);
        ctx.set_module_name(id, name);
        ctx.set_interface(id, check_result.interface.clone());
        ctx.set_name_supply(id, check_result.names);
        if parse_errors.is_empty() && check_result.errors.is_empty() && check_result.ir.is_none() {
            panic!("No IR generated, despite no errors")
        };
        checked_ids.push(id);
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

pub fn check_program_new(sources: &[(Utf8PathBuf, String)]) -> Result<(), String> {
    let check_result = run_frontend_new(sources);
    if let Some(count) = check_result.display_errors() {
        return Err(format!("Check failed with {} errors", count));
    }
    return Ok(());
}
/// Checks the given program, and prints any parse or type errors.
/// If there are any errors returns a summary message in Err otherwise Ok.
pub fn check_program(source: &str) -> Result<(), String> {
    let check_result = run_frontend(source);
    if check_result.has_errors() {
        let mut count = 0;
        for err in check_result.errors() {
            count += 1;
            eprintln!(
                "{}",
                err.display(&check_result.ctx, Utf8Path::new(""), source, true)
            );
        }
        return Err(format!("Check failed with {} errors", count));
    }
    Ok(())
}
