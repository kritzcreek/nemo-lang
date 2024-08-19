use std::path::Path;
use insta::glob;
use std::process::Command;

use insta_cmd::{assert_cmd_snapshot, get_cargo_bin};

fn compile_args(path: &Path) -> Vec<String> {
    vec![
        "compile".to_string(),
        path.display().to_string(),
        "--output".to_string(),
        format!("tests/build/passing/{}.wasm", path.file_stem().unwrap().to_str().unwrap())
    ]
}

fn run_args(path: &Path) -> Vec<String> {
    vec![
        "run".to_string(),
        "--allow-read".to_string(),
        "../../dev/wasm-runner.ts".to_string(),
        format!("tests/build/passing/{}.wasm", path.file_stem().unwrap().to_str().unwrap())
    ]
}

fn cli() -> Command {
    Command::new(get_cargo_bin("nemo"))
}

fn deno() -> Command {
    Command::new("deno")
}

fn run_test(path: &Path) {
    assert_cmd_snapshot!(cli().args(&compile_args(path)));
    assert_cmd_snapshot!(deno().args(&run_args(path)));

    // nemo compile $path --to build/test/passing/$path.wasm
    // deno run --allow-read dev/wasm-runner.ts build/test/passing/$path.wasm
}

#[test]
fn test_passing() {
    glob!("passing/*.nemo", |path| {
        run_test(path);
    });
    glob!("failing/*.nemo", |path| {
        run_test(path);
    });
}