use rowan::TextRange;
use std::collections::HashMap;
use topological_sort::TopologicalSort;

use crate::ir::ModuleId;

#[derive(Debug)]
pub enum SortResult {
    Cycle(Vec<ModuleId>),
    Duplicate(String),
    Sorted {
        sorted: Vec<ModuleId>,
        unknown_modules: Vec<(ModuleId, TextRange, String)>,
    },
}

#[derive(Debug)]
pub struct ModuleInfo<'a> {
    pub id: ModuleId,
    pub name: &'a str,
    pub uses: &'a [(TextRange, String)],
}

pub fn toposort_modules(modules: Vec<ModuleInfo>) -> SortResult {
    let mut module_name_map = HashMap::new();
    let mut module_id_map = HashMap::new();
    let mut unknown_modules = vec![];
    let mut ts: TopologicalSort<ModuleId> = TopologicalSort::new();
    for ModuleInfo { id, name, uses: _ } in &modules {
        ts.insert(*id);
        let prev = module_name_map.insert(*name, *id);
        if prev.is_some() {
            return SortResult::Duplicate(name.to_string());
        }
        module_id_map.insert(*id, *name);
    }
    for ModuleInfo { id, name: _, uses } in &modules {
        for (range, dep) in *uses {
            if let Some(import_id) = module_name_map.get(dep.as_str()) {
                ts.add_dependency(*import_id, *id)
            } else {
                unknown_modules.push((*id, *range, dep.clone()))
            };
        }
    }
    let sorted: Vec<ModuleId> = ts.collect();
    if sorted.len() < modules.len() {
        // TODO: Report which modules participate in cycle
        return SortResult::Cycle(vec![]);
    }
    SortResult::Sorted {
        sorted,
        unknown_modules,
    }
}
