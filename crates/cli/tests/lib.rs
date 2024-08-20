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

fn compile_args(path: &Path) -> (Vec<String>, String) {
    let out_path = format!(
        "tests/build/{}.wasm",
        path.file_stem().unwrap().to_str().unwrap()
    );
    (
        vec![
            "compile".to_string(),
            render_slash_path(path),
            "--output".to_string(),
            out_path.clone(),
        ],
        out_path,
    )
}

fn check_args(path: &Path) -> Vec<String> {
    vec!["check".to_string(), render_slash_path(path)]
}

fn cli() -> Command {
    Command::new(get_cargo_bin("nemo"))
}

fn run_args(path: String) -> Vec<String> {
    vec![
        "run".to_string(),
        "--allow-read".to_string(),
        "../../dev/wasm-runner.ts".to_string(),
        path,
    ]
}

fn deno() -> Command {
    Command::new("deno")
}

fn run_test(path: &Path) {
    let (args, out_path) = compile_args(path);
    assert_cmd_snapshot!(cli().args(args));
    assert_cmd_snapshot!(deno().env("NO_COLOR", "1").args(run_args(out_path)));
}

fn check_test(path: &Path) {
    assert_cmd_snapshot!(cli().args(check_args(path)));
}

fn clear_existing_build() {
    let _ = std::fs::remove_dir_all("tests/build");
    std::fs::create_dir_all("tests/build").unwrap();
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
    clear_existing_build();
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
