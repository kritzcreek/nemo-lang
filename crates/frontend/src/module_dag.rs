use rowan::TextRange;
use std::collections::HashMap;
use topological_sort::TopologicalSort;

use crate::ir::ModuleId;

#[derive(Debug)]
pub enum SortResult {
    Cycle(Vec<ModuleId>),
    Sorted {
        sorted: Vec<ModuleId>,
        unknown_modules: Vec<(ModuleId, TextRange, String)>,
    },
}

// TODO: Report errors for
// - Import cycles. Handle by early exit with error? Don't think we can recover gracefully here?
pub fn toposort_modules<'a>(
    modules: Vec<(ModuleId, &str, &'a [(TextRange, String)])>,
) -> SortResult {
    let mut module_name_map = HashMap::new();
    let mut module_id_map = HashMap::new();
    let mut unknown_modules = vec![];
    let mut ts: TopologicalSort<ModuleId> = TopologicalSort::new();
    for (id, name, _) in &modules {
        ts.insert(*id);
        module_name_map.insert(*name, *id);
        module_id_map.insert(*id, *name);
    }
    for (id, _, deps) in &modules {
        for (range, dep) in *deps {
            if let Some(import_id) = module_name_map.get(dep.as_str()) {
                ts.add_dependency(*import_id, *id)
            } else {
                unknown_modules.push((*id, *range, dep.clone()))
            };
        }
    }
    let sorted: Vec<ModuleId> = ts.collect();
    if sorted.len() < modules.len() {
        // TODO: Report cycle
        // let cycle = ts.find_cycle();
        return SortResult::Cycle(vec![]);
    }
    SortResult::Sorted {
        sorted,
        unknown_modules,
    }
}
