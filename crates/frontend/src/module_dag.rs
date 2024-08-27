use std::collections::HashMap;
use topological_sort::TopologicalSort;

use crate::ir::ModuleId;

// TODO: Report errors for
// - Unknown modules. Handle by recording the import somehow and giving all references to it type ERROR?
// - Import cycles. Handle by early exit with error? Don't think we can recover gracefully here?
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
            if let Some(import_id) = module_name_map.get(dep.as_str()) {
                ts.add_dependency(*import_id, *id)
            } else {
                // TODO: report error
            };
        }
    }

    ts.collect()
}
