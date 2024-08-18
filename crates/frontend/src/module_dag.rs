use std::collections::HashMap;
use topological_sort::TopologicalSort;

use crate::{
    ir::{ModuleId, ModuleIdGen},
    syntax::{Module, Root},
};

fn mod_name(module: &Module) -> String {
    module
        .mod_header()
        .and_then(|h| h.ident_token())
        .map(|t| t.text().to_string())
        .unwrap_or("DEFAULT_MODULE_NAME".to_string())
}

pub fn toposort_modules(root: Root) -> Vec<(ModuleId, String, Module)> {
    let mut id_gen = ModuleIdGen::new();
    let (module_name_map, mut module_id_map) = {
        let mut module_name_map = HashMap::new();
        let mut module_id_map = HashMap::new();
        for module in root.modules() {
            let id = id_gen.next_id();
            module_name_map.insert(mod_name(&module), id);
            module_id_map.insert(id, (mod_name(&module), module.clone()));
        }
        (module_name_map, module_id_map)
    };

    let mut ts: TopologicalSort<ModuleId> = TopologicalSort::new();
    for (_, module) in module_id_map.values() {
        let module_id = module_name_map[&mod_name(module)];
        ts.insert(module_id);
        let Some(header) = module.mod_header() else {
            continue;
        };
        for mod_use in header.mod_uses() {
            let import_name_tkn = mod_use.ident_token().unwrap();
            let import_id = module_name_map[import_name_tkn.text()];
            ts.add_dependency(import_id, module_id)
        }
    }
    ts.map(|id| {
        let (name, module) = module_id_map.remove(&id).unwrap();
        (id, name, module)
    })
    .collect()
}
