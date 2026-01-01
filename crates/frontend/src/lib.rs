pub mod builtins;
mod error;
pub mod highlight;
pub mod ir;
pub mod parser;
mod scheduler;
pub mod syntax;
pub mod types;

use camino::Utf8PathBuf;
pub use error::CheckError;
use ir::{Ctx, Id, ModuleId, ModuleIdGen, Program};
use parser::{parse_prog, ParseError};
use rayon::prelude::*;
use rowan::GreenNode;
use std::{collections::HashMap, fs};
use syntax::Module;
pub use types::CheckResult;
use types::{OccurrenceMap, TyError};

#[derive(Debug)]
pub struct FrontendResult {
    pub ctx: Ctx,
    pub modules: Vec<ModuleResult>,
}

impl FrontendResult {
    pub fn errors(&self) -> impl Iterator<Item = CheckError<'_>> {
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
            let path = self.ctx.get_module_path(module.parse_result.id);
            for error in &module.parse_result.parse_errors {
                count += 1;
                println!("{}", error.display(path, &module.parse_result.source));
            }
            for error in &module.ty_errors {
                count += 1;
                println!(
                    "{}",
                    error.display(&self.ctx, path, &module.parse_result.source)
                );
            }
        }
        if count > 0 {
            Some(count)
        } else {
            None
        }
    }

    pub fn consume(self) -> (Ctx, ir::Program) {
        let mut ir = ir::Program::default();
        for module in self.modules {
            ir.merge(module.ir)
        }
        (self.ctx, ir)
    }
}

#[derive(Debug)]
pub struct ModuleResult {
    pub parse_result: ModuleParseResult,
    pub occurrences: OccurrenceMap,
    pub ty_errors: Vec<TyError>,
    pub ir: Program,
}

#[derive(Debug)]
pub struct ModuleParseResult {
    pub id: ModuleId,
    pub parse_errors: Vec<ParseError>,
    pub source: String,
    pub parse: GreenNode,
    pub path: Utf8PathBuf,
    pub name: String,
    pub dependencies: Vec<Id>,
}

fn module_name(module: &Module) -> Option<String> {
    Some(module.mod_header()?.ident_token()?.text().to_string())
}

fn extract_module_header(module: &Module) -> (String, Vec<Id>) {
    let name = module_name(module).unwrap_or_default();
    let dependencies = module
        .mod_uses()
        .filter_map(|mod_use| {
            mod_use.ident_token().map(|t| Id {
                it: t.text().to_string(),
                at: t.text_range(),
            })
        })
        .filter(|mod_use| {
            mod_use.it != "prim"
        })
        .collect();
    (name, dependencies)
}

fn parse_module(
    path: &Utf8PathBuf,
    id: ModuleId,
    source: String,
) -> Result<ModuleParseResult, String> {
    let parse_result = parse_prog(&source);
    let (name, dependencies) = extract_module_header(&parse_result.root().module().unwrap());
    let (parse, parse_errors) = parse_result.take();
    Ok(ModuleParseResult {
        id,
        source,
        name,
        dependencies,
        parse_errors,
        parse,
        path: path.clone(),
    })
}

fn parse_paths(
    paths: &[Utf8PathBuf],
    worker_count: usize,
) -> Vec<Result<ModuleParseResult, String>> {
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(worker_count)
        .build()
        .unwrap();
    let id_gen = ModuleIdGen::new();
    pool.install(|| {
        paths
            .into_par_iter()
            .map(|path| -> Result<ModuleParseResult, String> {
                let bytes = fs::read(path).map_err(|e| e.to_string())?;
                let source = String::from_utf8(bytes).map_err(|e| e.to_string())?;
                let id = id_gen.next_id();
                parse_module(path, id, source)
            })
            .collect()
    })
}

pub fn parse_sources(
    sources: Vec<(Utf8PathBuf, String)>,
) -> Vec<Result<ModuleParseResult, String>> {
    let id_gen = ModuleIdGen::new();
    sources
        .into_iter()
        .map(|(path, source)| parse_module(&path, id_gen.next_id(), source))
        .collect()
}

fn check_modules(
    parse_results: Vec<Result<ModuleParseResult, String>>,
    worker_count: usize,
) -> Result<FrontendResult, String> {
    let mut parsed_modules: HashMap<ModuleId, ModuleParseResult> = HashMap::new();
    let mut module_lookup: HashMap<String, ModuleId> = HashMap::new();
    for parse_result in parse_results {
        match parse_result {
            Ok(res) => {
                if module_lookup.insert(res.name.clone(), res.id).is_some() {
                    return Err(format!("Duplicate module name declared: '{}'", res.name));
                };
                assert!(parsed_modules.insert(res.id, res).is_none());
            }
            Err(err) => return Err(err),
        }
    }
    let mut unknown_modules = vec![];
    let scheduler_input: HashMap<ModuleId, Vec<ModuleId>> = parsed_modules
        .values()
        .map(|res| {
            let deps: Vec<ModuleId> = res
                .dependencies
                .iter()
                .filter_map(|dep| {
                    let Some(id) = module_lookup.get(&dep.it) else {
                        unknown_modules.push((res.id, dep.clone()));
                        return None;
                    };
                    Some(*id)
                })
                .collect();
            (res.id, deps)
        })
        .collect();
    for (mid, um) in unknown_modules {
        parsed_modules
            .get_mut(&mid)
            .unwrap()
            .parse_errors
            .push(ParseError {
                it: format!("Unknown module '{}'", um.it),
                at: um.at,
            });
    }
    let mut inverted_module_lookup = HashMap::new();
    for (k, v) in &module_lookup {
        inverted_module_lookup.insert(*v, k.as_str());
    }
    let state = scheduler::make_state(scheduler_input, inverted_module_lookup)?;
    if worker_count == 1 {
        Ok(scheduler::run_single_threaded(
            state,
            module_lookup,
            parsed_modules,
        ))
    } else {
        Ok(scheduler::run(
            state,
            module_lookup,
            parsed_modules,
            worker_count,
        ))
    }
}

pub fn run_frontend(
    sources: &[Utf8PathBuf],
    worker_count: usize,
) -> Result<FrontendResult, String> {
    check_modules(parse_paths(sources, worker_count), worker_count)
}

pub fn run_frontend_pure(sources: Vec<(Utf8PathBuf, String)>) -> Result<FrontendResult, String> {
    check_modules(parse_sources(sources), 1)
}

/// Checks the given sources and prints any parse or type errors.
/// If there are any errors returns a summary message in Err otherwise Ok.
pub fn check_program(sources: &[Utf8PathBuf], worker_count: usize) -> Result<(), String> {
    let check_result = run_frontend(sources, worker_count)?;
    if let Some(count) = check_result.display_errors() {
        return Err(format!("Check failed with {count} errors"));
    }
    Ok(())
}
