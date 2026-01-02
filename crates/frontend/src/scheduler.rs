use std::{
    cmp::min,
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
    thread,
};

use crossbeam_channel::unbounded;
use crossbeam_deque::Injector;

use crate::{
    FrontendResult, ModuleParseResult, ModuleResult,
    ir::{Ctx, ModuleId, Program},
    syntax::Module,
    types::{self, OccurrenceMap, TyError},
};

pub struct State {
    pub in_degrees: HashMap<ModuleId, u32>,
    pub dependents: HashMap<ModuleId, Vec<ModuleId>>,
}

impl State {
    pub fn finish(&mut self, id: ModuleId) -> Vec<ModuleId> {
        let mut ready = vec![];
        let Some(dependents) = self.dependents.get(&id) else {
            return vec![];
        };
        for dependent in dependents {
            let i = self
                .in_degrees
                .entry(*dependent)
                .and_modify(|f| *f -= 1)
                .or_default();
            if *i == 0 {
                ready.push(*dependent)
            }
        }
        ready
    }
}

pub fn make_state(
    modules: HashMap<ModuleId, Vec<ModuleId>>,
    module_lookup: HashMap<ModuleId, &str>,
) -> Result<State, String> {
    let mut state = State {
        in_degrees: HashMap::new(),
        dependents: HashMap::new(),
    };
    let sccs = TarjanSCC::new(&modules);
    if sccs.has_cycles() {
        let mut err = String::new();
        for mut c in sccs.get_cycles() {
            // Make cycle error output deterministic by rotating the smallest module (by name) to the front
            let mut iter = c.iter().map(|i| module_lookup.get(i).unwrap()).enumerate();
            let (mut min_ix, mut min) = iter.next().unwrap();
            for (ix, id) in iter {
                if id < min {
                    min = id;
                    min_ix = ix;
                }
            }
            c.rotate_left(min_ix);
            let s: String = c
                .iter()
                .map(|i| *module_lookup.get(i).unwrap())
                .collect::<Vec<_>>()
                .join(" -> ");
            err += "\n";
            err += &s;
        }
        return Err(format!("Cycle detected in module dependencies:{err}"));
    }

    for (id, deps) in modules {
        let dep_count = deps.len() as u32;
        state.in_degrees.insert(id, dep_count);
        for d in deps {
            state
                .dependents
                .entry(d)
                .and_modify(|ms| ms.push(id))
                .or_insert(vec![id]);
        }
    }

    Ok(state)
}

// Implementation copied from Claude
pub struct TarjanSCC {
    index: usize,
    stack: Vec<ModuleId>,
    on_stack: HashMap<ModuleId, bool>,
    indices: HashMap<ModuleId, usize>,
    low_links: HashMap<ModuleId, usize>,
    sccs: Vec<Vec<ModuleId>>,
}

impl TarjanSCC {
    pub fn new(graph: &HashMap<ModuleId, Vec<ModuleId>>) -> Self {
        let mut scc = Self {
            index: 0,
            stack: Vec::new(),
            on_stack: HashMap::new(),
            indices: HashMap::new(),
            low_links: HashMap::new(),
            sccs: Vec::new(),
        };
        scc.find_sccs(graph);
        scc
    }

    fn find_sccs(&mut self, graph: &HashMap<ModuleId, Vec<ModuleId>>) {
        // Initialize all nodes
        for node in graph.keys() {
            if !self.indices.contains_key(node) {
                self.strongconnect(*node, graph);
            }
        }
    }

    fn strongconnect(&mut self, v: ModuleId, graph: &HashMap<ModuleId, Vec<ModuleId>>) {
        // Set the depth index for v to the smallest unused index
        self.indices.insert(v, self.index);
        self.low_links.insert(v, self.index);
        self.index += 1;
        self.stack.push(v);
        self.on_stack.insert(v, true);

        // Consider successors of v
        if let Some(successors) = graph.get(&v) {
            for &w in successors {
                if !self.indices.contains_key(&w) {
                    // Successor w has not yet been visited; recurse on it
                    self.strongconnect(w, graph);
                    let v_low = *self.low_links.get(&v).unwrap();
                    let w_low = *self.low_links.get(&w).unwrap();
                    self.low_links.insert(v, min(v_low, w_low));
                } else if *self.on_stack.get(&w).unwrap_or(&false) {
                    // Successor w is in stack and hence in the current SCC
                    let v_low = *self.low_links.get(&v).unwrap();
                    let w_idx = *self.indices.get(&w).unwrap();
                    self.low_links.insert(v, min(v_low, w_idx));
                }
            }
        }

        // If v is a root node, pop the stack and create an SCC
        if self.low_links.get(&v) == self.indices.get(&v) {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().unwrap();
                self.on_stack.insert(w, false);
                scc.push(w);
                if w == v {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }

    pub fn has_cycles(&self) -> bool {
        self.sccs.iter().any(|scc| scc.len() > 1)
    }

    pub fn get_cycles(&self) -> Vec<Vec<ModuleId>> {
        self.sccs
            .iter()
            .filter(|scc| scc.len() > 1)
            .cloned()
            .collect()
    }
}

#[derive(Debug)]
pub struct TypeCheckResult {
    pub id: ModuleId,
    pub occurrences: OccurrenceMap,
    pub ty_errors: Vec<TyError>,
    pub ir: Program,
}

pub fn run(
    mut state: State,
    module_lookup: HashMap<String, ModuleId>,
    mut parsed_modules: HashMap<ModuleId, ModuleParseResult>,
    num_workers: usize,
) -> FrontendResult {
    let count = module_lookup.len();
    let ctx = Ctx::new(module_lookup);

    let injector: Injector<ModuleId> = Injector::new();
    let (completion_tx, completion_rx) = unbounded::<TypeCheckResult>();
    for (m, in_deg) in &state.in_degrees {
        if *in_deg == 0 {
            injector.push(*m);
        }
    }

    let remaining = AtomicUsize::new(count);
    let checked_modules = thread::scope(|s| {
        for _ in 0..num_workers {
            let completion_tx = completion_tx.clone();
            let injector = &injector;
            let parsed_modules = &parsed_modules;
            let ctx = &ctx;
            let remaining = &remaining;
            s.spawn(move || {
                loop {
                    let task = std::iter::repeat_with(|| injector.steal())
                        .find(|s| !s.is_retry())
                        .and_then(|s| s.success());
                    match task {
                        Some(id) => {
                            let parsed_module = parsed_modules.get(&id).unwrap();
                            let res = types::check_module(
                                ctx,
                                Module::from_root(parsed_module.parse.clone()),
                                id,
                            );
                            ctx.set_module(
                                id,
                                parsed_module.name.clone(),
                                parsed_module.path.clone(),
                                res.interface,
                                res.names,
                            );
                            let module_result = TypeCheckResult {
                                id,
                                occurrences: res.occurrences,
                                ty_errors: res.errors,
                                ir: res.ir,
                            };
                            completion_tx.send(module_result).unwrap();
                        }
                        None => {
                            if remaining.load(Ordering::Acquire) == 0 {
                                break;
                            }
                            std::hint::spin_loop();
                        }
                    }
                }
            });
        }
        drop(completion_tx);

        let mut checked_modules = vec![];
        while let Ok(result) = completion_rx.recv() {
            let id = result.id;
            checked_modules.push(result);
            let prev = remaining.fetch_sub(1, Ordering::AcqRel);
            if prev == 1 {
                break;
            }
            for dep_id in state.finish(id) {
                injector.push(dep_id);
            }
        }
        checked_modules
    });
    FrontendResult {
        ctx,
        modules: checked_modules
            .into_iter()
            .map(|cm| ModuleResult {
                parse_result: parsed_modules.remove(&cm.id).unwrap(),
                occurrences: cm.occurrences,
                ty_errors: cm.ty_errors,
                ir: cm.ir,
            })
            .collect(),
    }
}

pub fn run_single_threaded(
    mut state: State,
    module_lookup: HashMap<String, ModuleId>,
    mut parsed_modules: HashMap<ModuleId, ModuleParseResult>,
) -> FrontendResult {
    let ctx = Ctx::new(module_lookup);
    let mut ready = vec![];

    for (m, in_deg) in &state.in_degrees {
        if *in_deg == 0 {
            ready.push(*m);
        }
    }

    let mut checked_modules = vec![];
    while let Some(id) = ready.pop() {
        let parsed_module = parsed_modules.get(&id).unwrap();
        let res = types::check_module(&ctx, Module::from_root(parsed_module.parse.clone()), id);
        ctx.set_module(
            id,
            parsed_module.name.clone(),
            parsed_module.path.clone(),
            res.interface,
            res.names,
        );
        let module_result = TypeCheckResult {
            id,
            occurrences: res.occurrences,
            ty_errors: res.errors,
            ir: res.ir,
        };
        checked_modules.push(module_result);
        ready.extend(state.finish(id));
    }

    FrontendResult {
        ctx,
        modules: checked_modules
            .into_iter()
            .map(|cm| ModuleResult {
                parse_result: parsed_modules.remove(&cm.id).unwrap(),
                occurrences: cm.occurrences,
                ty_errors: cm.ty_errors,
                ir: cm.ir,
            })
            .collect(),
    }
}
