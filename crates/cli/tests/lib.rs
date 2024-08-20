use insta::{glob, Settings};
use std::path::Path;
use std::process::Command;

use insta_cmd::{assert_cmd_snapshot, get_cargo_bin};

fn render_slash_path(path: &Path) -> String {
    assert!(path.is_relative());
    path.components()
        .map(|c| c.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

fn compile_args(path: &Path) -> Vec<String> {
    vec![
        "compile".to_string(),
        render_slash_path(path),
        "--output".to_string(),
        format!(
            "tests/build/passing/{}.wasm",
            path.file_stem().unwrap().to_str().unwrap()
        ),
    ]
}

fn check_args(path: &Path) -> Vec<String> {
    vec!["check".to_string(), render_slash_path(path)]
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
    let cwd = std::env::current_dir().unwrap();
    apply_common_filters!();
    glob!("check/**/*.nemo", |path| {
        let path = path.strip_prefix(&cwd).unwrap();
        check_test(path);
    });
    glob!("run/**/*.nemo", |path| {
        let path = path.strip_prefix(&cwd).unwrap();
        run_test(path);
    });
}
