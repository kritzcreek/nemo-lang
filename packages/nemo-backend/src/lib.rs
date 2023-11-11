use lower::Lower;
use nemo_frontend::syntax;

pub mod ir;
pub mod lower;

pub fn lower(program: syntax::Program) -> ir::Program {
    let lower = Lower::new();
    lower.rename_program(program).0
}
