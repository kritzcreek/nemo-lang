use frontend::ir::Program;

mod compute_size;

pub fn run_passes(p: &Program) {
    compute_size::compute_sizes(p);
}
