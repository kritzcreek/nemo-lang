use std::collections::HashMap;
use topological_sort::TopologicalSort;

use crate::ir::ModuleId;

pub fn toposort_modules(modules: Vec<(ModuleId, &str, &[String])>) -> Vec<ModuleId> {
    let mut module_name_map = HashMap::new();
    let mut module_id_map = HashMap::new();
    let mut ts: TopologicalSort<ModuleId> = TopologicalSort::new();

    for (id, name, _) in &modules {
        ts.insert(*id);
        module_name_map.insert(*name, *id);
        module_id_map.insert(*id, *name);
    }

    for (id, _, deps) in &modules {
        for dep in *deps {
            let import_id = module_name_map[dep.as_str()];
            ts.add_dependency(import_id, *id)
        }
    }
    ts.collect()
}
