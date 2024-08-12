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
        .unwrap()
        .text()
        .to_string()
}

pub fn toposort_modules(root: Root) -> Vec<(ModuleId, Module)> {
    let mut id_gen = ModuleIdGen::new();
    let (module_name_map, mut module_id_map) = {
        let mut module_name_map = HashMap::new();
        let mut module_id_map = HashMap::new();
        for module in root.modules() {
            let id = id_gen.next();
            module_name_map.insert(mod_name(&module), id);
            module_id_map.insert(id, module.clone());
        }
        (module_name_map, module_id_map)
    };

    let mut ts: TopologicalSort<ModuleId> = TopologicalSort::new();
    for module in module_id_map.values() {
        let module_id = module_name_map[&mod_name(module)];
        for mod_use in module.mod_header().unwrap().mod_uses() {
            let import_name_tkn = mod_use.ident_token().unwrap();
            let import_id = module_name_map[import_name_tkn.text()];
            ts.add_dependency(module_id, import_id)
        }
    }
    ts.into_iter()
        .map(|id| (id, module_id_map.remove(&id).unwrap()))
        .collect()
}
