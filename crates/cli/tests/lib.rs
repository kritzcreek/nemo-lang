use camino::{Utf8Path, Utf8PathBuf};
use insta::{glob, Settings};
use insta_cmd::{assert_cmd_snapshot, get_cargo_bin};
use std::process::Command;
use std::{fs, path::Path};

fn render_slash_path(path: &Utf8Path) -> String {
    assert!(path.is_relative());
    path.components()
        .map(|c| c.as_str())
        .collect::<Vec<_>>()
        .join("/")
}

fn compile_args(paths: &[Utf8PathBuf], out_name: &str) -> (Vec<String>, String) {
    let out_path = format!("tests/build/{}.wasm", out_name);
    let mut args = vec![
        "compile".to_string(),
        "--output".to_string(),
        out_path.clone(),
    ];
    args.extend(paths.iter().map(|p| render_slash_path(p)));
    (args, out_path)
}

fn check_args(paths: &[Utf8PathBuf]) -> Vec<String> {
    let mut args = vec!["check".to_string()];
    args.extend(paths.iter().map(|p| render_slash_path(p)));
    args
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

fn collect_test_input(path: &Path) -> (String, Vec<Utf8PathBuf>) {
    let path = Utf8Path::from_path(path).expect("Non-utf8 path: {path:?}");
    let out_name = path.file_stem().unwrap();
    let mut paths = vec![];
    if path.is_dir() {
        for entry in fs::read_dir(path).expect("Failed to list directory {path:?}") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            paths.push(Utf8PathBuf::from_path_buf(path).expect("non-utf8 path: {path:?}"));
        }
    } else {
        paths.push(path.to_owned());
    };
    (out_name.to_string(), paths)
}

fn run(path: &Path) {
    let (out_name, paths) = collect_test_input(path);
    let (args, out_path) = compile_args(&paths, &out_name);
    assert_cmd_snapshot!(cli().env("RUST_BACKTRACE", "0").args(args));
    assert_cmd_snapshot!(deno().env("NO_COLOR", "1").args(run_args(out_path)));
}

fn check(path: &Path) {
    let (_, paths) = collect_test_input(path);
    assert_cmd_snapshot!(cli().args(check_args(&paths)));
}

fn clear_existing_build() {
    let _ = std::fs::remove_dir_all("tests/build");
    std::fs::create_dir_all("tests/build").unwrap();
}

macro_rules! apply_common_filters {
    {} => {
        let mut settings = Settings::clone_current();
        settings.set_prepend_module_to_snapshot(false);
        // Removes deno stack traces, as these change between versions
        settings.add_filter(r"    at(.*)", "");
        let _bound = settings.bind_to_scope();
    }
}

#[test]
fn t() {
    let cwd = std::env::current_dir().unwrap();
    clear_existing_build();
    apply_common_filters!();
    glob!("check/*", |path| {
        let path = path.strip_prefix(&cwd).unwrap();
        check(path);
    });
    glob!("run/*", |path| {
        let path = path.strip_prefix(&cwd).unwrap();
        run(path);
    });
}
