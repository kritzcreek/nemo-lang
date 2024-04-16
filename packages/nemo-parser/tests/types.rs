use insta::{assert_snapshot, glob};
use nemo_parser::check_program;
use nemo_parser::types::errors::render_ty_error;
use std::fmt::Write;
use std::fs;
use std::path::Path;
use yansi::Paint;

fn check_failing(path: &Path, source: String) {
    let errors = check_program(&source);
    if errors.is_empty() {
        eprintln!("{} was expected to fail, but didn't", path.display())
    }
    let mut err_buf = String::new();
    for error in errors {
        writeln!(&mut err_buf, "{}", render_ty_error(&source, &error, false)).unwrap()
    }
    assert_snapshot!(err_buf)
}

#[test]
fn test_failing() {
    Paint::disable();
    glob!("type_errors", "*.nemo", |path| {
        let input = fs::read_to_string(path).unwrap();
        check_failing(path, input)
    });
}
