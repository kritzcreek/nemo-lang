pub mod builtins;
pub mod parser;
pub mod pretty;
pub mod syntax;
pub mod type_errors;
pub mod types;

use parser::parse_program;
use types::typecheck;

pub fn check_program(program: &str) -> types::TyResult<syntax::Program> {
    let parse_tree = parse_program(program);
    typecheck(program, parse_tree.root_node())
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;

    use crate::type_errors::render_ty_error;

    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use insta::{assert_snapshot, glob};

    fn check_failing(path: &Path, source: String) {
        match check_program(&source) {
            Ok(_) => panic!("{} was expected to fail, but didn't", path.display()),
            Err(err) => assert_snapshot!(
                format!("{}", path.display()),
                render_ty_error(&source, &err)
            ),
        }
    }

    #[test]
    fn test_failing() {
        glob!("../test_data", "failing/*.nemo", |path| {
            let input = fs::read_to_string(path).unwrap();
            check_failing(path, input)
        });
    }
}
