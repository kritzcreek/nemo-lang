use anyhow::Result;
use backend::compile_program;
use camino::Utf8PathBuf;
use clap::{Parser, Subcommand};
use std::{error::Error, fs, thread};
use wasmtime::{Config, Engine, Instance, Linker, Module, Store, TypedFunc};

/// The nemo language
#[derive(Debug, Parser)]
#[command(name = "nemo")]
#[command(about = "The nemo language CLI", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Compiles Nemo programs into a *.wasm module
    Compile {
        /// The *.nemo files to check
        input_files: Vec<Utf8PathBuf>,

        /// Output Wasm with the given filename
        #[arg(long)]
        output: Utf8PathBuf,

        /// Number of jobs to be run in parallel
        #[arg(short, long)]
        jobs: Option<usize>,
    },
    /// Checks Nemo programs and reports any errors. Does not generate Wasm.
    Check {
        /// The *.nemo files to check
        input_files: Vec<Utf8PathBuf>,

        /// Number of jobs to be run in parallel
        #[arg(short, long)]
        jobs: Option<usize>,
    },
    Run {
        /// The *.nemo files to check
        input_files: Vec<Utf8PathBuf>,

        /// Number of jobs to be run in parallel
        #[arg(short, long)]
        jobs: Option<usize>,
    },
    /// Runs the language server
    LanguageServer {
        #[arg(long)]
        stdio: bool,
    },
}

fn find_wasm_main<T>(instance: &Instance, mut store: &mut Store<T>) -> Result<TypedFunc<(), i32>> {
    let main_export = instance
        .exports(&mut store)
        .find(|export| export.name() == "main" || export.name().ends_with("::main"))
        .unwrap();
    main_export.into_func().unwrap().typed::<(), i32>(store)
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let args = Cli::parse();
    match args.command {
        Commands::Compile {
            input_files,
            output,
            jobs,
        } => {
            let worker_count =
                jobs.unwrap_or_else(|| (thread::available_parallelism().unwrap().get() / 2).max(1));
            let wasm = compile_program(&input_files, worker_count)?;
            fs::write(output, wasm)?;
            Ok(())
        }
        Commands::Check { input_files, jobs } => {
            let worker_count =
                jobs.unwrap_or_else(|| (thread::available_parallelism().unwrap().get() / 2).max(1));
            frontend::check_program(&input_files, worker_count)?;
            Ok(())
        }
        Commands::LanguageServer { .. } => {
            // start_language_server()
            todo!();
        }
        Commands::Run { input_files, jobs } => {
            let worker_count =
                jobs.unwrap_or_else(|| (thread::available_parallelism().unwrap().get() / 2).max(1));
            let wasm = compile_program(&input_files, worker_count)?;
            let engine = Engine::new(Config::new().wasm_function_references(true).wasm_gc(true))?;
            let mut store = Store::new(&engine, ());
            let mut linker = Linker::new(&engine);
            let module = Module::new(&engine, &wasm)?;
            linker.func_wrap("env", "log", |param: i32| {
                println!("{param}");
                0
            })?;
            linker.func_wrap("env", "log_int", |param: i32| {
                println!("{param}");
                0
            })?;
            linker.func_wrap("env", "log_float", |param: f32| {
                println!("{param}");
                0
            })?;
            linker.func_wrap("env", "print_char", |param: u32| {
                print!("{}", char::from_u32(param).unwrap());
                0
            })?;
            linker.func_wrap("env", "random", |_: i32| -> f32 { 0.0 })?;

            let instance = linker.instantiate(&mut store, &module)?;
            let main_func = find_wasm_main(&instance, &mut store)?;
            let result = main_func.call(&mut store, ())?;
            println!("Result: {result:?}");
            Ok(())
        }
    }
}
