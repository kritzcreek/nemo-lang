use backend::compile_program;
use camino::{Utf8Path, Utf8PathBuf};
use insta::{assert_snapshot, glob};
use insta_cmd::{assert_cmd_snapshot, get_cargo_bin};
use std::fmt;
use std::process::Command;
use std::{fs, path::Path};
use wasmtime::{Caller, Config, Engine, Linker, Module, Store};

fn read_source_files(
    input_files: Vec<Utf8PathBuf>,
) -> Result<Vec<(Utf8PathBuf, String)>, std::io::Error> {
    input_files
        .into_iter()
        .map(|input_file| {
            let source = fs::read_to_string(&input_file)?;
            Ok((input_file, source))
        })
        .collect()
}

fn render_slash_path(path: &Utf8Path) -> String {
    assert!(path.is_relative());
    path.components()
        .map(|c| c.as_str())
        .collect::<Vec<_>>()
        .join("/")
}

fn check_args(paths: &[Utf8PathBuf]) -> Vec<String> {
    let mut args = vec!["check".to_string()];
    args.extend(paths.iter().map(|p| render_slash_path(p)));
    args
}

fn cli() -> Command {
    Command::new(get_cargo_bin("nemo"))
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

enum Run {
    Success { result: i32, output: Vec<String> },
    Trap { reason: String, output: Vec<String> },
}

impl fmt::Display for Run {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Run::Success { result, output } => {
                writeln!(f, "Success: {result}")?;
                for out in output {
                    writeln!(f, "{out}")?;
                }
            }
            Run::Trap { reason, output } => {
                writeln!(f, "Trapped: {reason}")?;
                for out in output {
                    writeln!(f, "{out}")?;
                }
            }
        };
        Ok(())
    }
}

fn wasmtime_runner(wasm: Vec<u8>) -> Run {
    let engine = Engine::new(Config::new().wasm_function_references(true).wasm_gc(true)).unwrap();
    let mut store = Store::new(&engine, vec![]);
    let mut linker = Linker::new(&engine);
    let module = Module::new(&engine, &wasm).unwrap();
    linker
        .func_wrap(
            "env",
            "log",
            |mut caller: Caller<'_, Vec<String>>, param: i32| {
                caller.data_mut().push(format!("{param}"));
                0
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "log_int",
            |mut caller: Caller<'_, Vec<String>>, param: i32| {
                caller.data_mut().push(format!("{param}"));
                0
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "log_float",
            |mut caller: Caller<'_, Vec<String>>, param: f32| {
                caller.data_mut().push(format!("{param}"));
                0
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "log_f32",
            |mut caller: Caller<'_, Vec<String>>, param: f32| {
                caller.data_mut().push(format!("{param}"));
                0
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "print_char",
            |mut caller: Caller<'_, Vec<String>>, param: u32| {
                let mut last_line = caller.data_mut().pop().unwrap_or_default();
                last_line.push_str(&format!("{}", char::from_u32(param).unwrap()));
                caller.data_mut().push(last_line);
                0
            },
        )
        .unwrap();
    linker
        .func_wrap("env", "random", |_: i32| -> f32 { 0.0 })
        .unwrap();

    let instance = linker.instantiate(&mut store, &module).unwrap();
    let main_export = instance
        .exports(&mut store)
        .find(|export| export.name().ends_with("main"))
        .unwrap();
    let main_func = main_export
        .into_func()
        .unwrap()
        .typed::<(), i32>(&mut store)
        .unwrap();
    match main_func.call(&mut store, ()) {
        Ok(result) => Run::Success {
            result,
            output: store.into_data(),
        },
        Err(err) => Run::Trap {
            reason: err.to_string(),
            output: store.into_data(),
        },
    }
}

fn test_run(path: &Path) {
    let (_, paths) = collect_test_input(path);
    let sources = read_source_files(paths).unwrap();
    let wasm = compile_program(&sources).unwrap();
    let output = wasmtime_runner(wasm);
    assert_snapshot!(output)
}

fn test_check(path: &Path) {
    let (_, paths) = collect_test_input(path);
    assert_cmd_snapshot!(cli().env("RUST_BACKTRACE", "0").args(check_args(&paths)));
}

#[test]
fn check() {
    let cwd = std::env::current_dir().unwrap();
    glob!("check/*", |path| {
        let path = path.strip_prefix(&cwd).unwrap();
        test_check(path);
    });
}

#[test]
fn run() {
    let cwd = std::env::current_dir().unwrap();
    glob!("run/*", |path| {
        let path = path.strip_prefix(&cwd).unwrap();
        test_run(path);
    });
}
