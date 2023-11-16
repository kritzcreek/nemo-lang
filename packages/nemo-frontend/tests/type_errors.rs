use insta::{assert_snapshot, glob};
use nemo_frontend::{check_program, type_errors};
use std::fs;
use std::path::Path;
use yansi::Paint;

fn check_failing(path: &Path, source: String) {
    match check_program(&source) {
        Ok(_) => panic!("{} was expected to fail, but didn't", path.display()),
        Err(err) => assert_snapshot!(type_errors::render_ty_error(&source, &err, false)),
    }
}

#[test]
fn test_failing() {
    Paint::disable();
    glob!("../test_data", "failing/*.nemo", |path| {
        let input = fs::read_to_string(path).unwrap();
        check_failing(path, input)
    });
}
