use insta::{glob, Settings};
use std::path::Path;
use std::process::Command;

use insta_cmd::{assert_cmd_snapshot, get_cargo_bin};

fn compile_args(path: &Path) -> Vec<String> {
    vec![
        "compile".to_string(),
        path.display().to_string(),
        "--output".to_string(),
        format!(
            "tests/build/passing/{}.wasm",
            path.file_stem().unwrap().to_str().unwrap()
        ),
    ]
}

fn check_args(path: &Path) -> Vec<String> {
    vec!["check".to_string(), path.display().to_string()]
}

fn cli() -> Command {
    Command::new(get_cargo_bin("nemo"))
}

fn run_args(path: &Path) -> Vec<String> {
    vec![
        "run".to_string(),
        "--allow-read".to_string(),
        "../../dev/wasm-runner.ts".to_string(),
        format!(
            "tests/build/passing/{}.wasm",
            path.file_stem().unwrap().to_str().unwrap()
        ),
    ]
}

fn deno() -> Command {
    Command::new("deno")
}

fn run_test(path: &Path) {
    assert_cmd_snapshot!(cli().args(compile_args(path)));
    assert_cmd_snapshot!(deno().env("NO_COLOR", "1").args(run_args(path)));
}

fn check_test(path: &Path) {
    assert_cmd_snapshot!(cli().args(check_args(path)));
}

macro_rules! apply_common_filters {
    {} => {
        let mut settings = Settings::clone_current();
        settings.add_filter(r"file:(.*)", "[FILE_PATH]");
        let _bound = settings.bind_to_scope();
    }
}

#[test]
fn t() {
    apply_common_filters!();
    glob!("check/**/*.nemo", |path| {
        check_test(path);
    });
    glob!("run/**/*.nemo", |path| {
        run_test(path);
    });
}
