pub mod builtins;
mod error;
pub mod highlight;
pub mod ir;
pub mod parser;
pub mod syntax;
pub mod types;

use parser::parse_prog;

pub use error::CheckError;
pub use types::CheckResult;

/// Runs the full frontend on `source` and returns the generated IR and other structures.
/// If there are any errors, the generated IR should _not_ be used. It's returned here for
/// debugging purposes.
pub fn run_frontend(source: &str) -> CheckResult<CheckError> {
    let (parse_root, parse_errors) = parse_prog(source).take();
    let mut errors = vec![];
    let check_result = types::check_prog(parse_root);

    for error in parse_errors {
        errors.push(CheckError::ParseError(error));
    }
    for error in check_result.errors {
        errors.push(CheckError::TypeError(error));
    }
    if errors.is_empty() && check_result.ir.is_none() {
        panic!("No IR generated, despite no errors")
    };

    CheckResult {
        errors,
        names: check_result.names,
        occurrences: check_result.occurrences,
        ir: check_result.ir,
        parse: check_result.parse,
    }
}

/// Checks the given program, and prints any parse or type errors.
/// If there are any errors returns a summary message in Err otherwise Ok.
pub fn check_program(source: &str) -> Result<(), String> {
    let check_result = run_frontend(source);
    if !check_result.errors.is_empty() {
        for err in &check_result.errors {
            eprintln!(
                "{}",
                err.display(source, &check_result.names.name_map, true)
            );
        }
        return Err(format!(
            "Check failed with {} errors",
            check_result.errors.len()
        ));
    }
    Ok(())
}
