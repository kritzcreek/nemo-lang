use frontend::check_program;
use insta::{assert_snapshot, glob};
use std::fmt::Write;
use std::fs;
use std::path::Path;
use std::str;
use yansi::Paint;

fn check_failing(path: &Path, source: String) {
    let (name_map, errors) = check_program(&source);
    if errors.is_empty() {
        eprintln!("{} was expected to fail, but didn't", path.display())
    }

    let mut err_buf = String::new();
    for error in errors {
        write!(&mut err_buf, "{}", error.display(&source, &name_map)).unwrap();
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
