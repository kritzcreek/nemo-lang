use backend::compile_program;
use camino::Utf8PathBuf;
use clap::{Parser, Subcommand};
use frontend::scip::write_index;
use std::{error::Error, fs, io};
use wasmtime::{Config, Engine, Store, Module, Instance};

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
    },
    /// Checks Nemo programs and reports any errors. Does not generate Wasm.
    Check {
        /// The *.nemo files to check
        input_files: Vec<Utf8PathBuf>,
    },
    Run {
        /// The *.nemo files to check
        input_files: Vec<Utf8PathBuf>,
    },
    /// Produces a SCIP index
    Index {
        #[arg(required = true)]
        /// The *.nemo files to index
        input_files: Vec<Utf8PathBuf>,
    },
    /// Runs the language server
    LanguageServer {
        #[arg(long)]
        stdio: bool,
    },
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let args = Cli::parse();
    match args.command {
        Commands::Compile {
            input_files,
            output,
        } => {
            let sources: Vec<_> = input_files
                .into_iter()
                .map(|input_file| {
                    let source = fs::read_to_string(&input_file)?;
                    Ok((input_file, source))
                })
                .collect::<Result<Vec<_>, io::Error>>()?;
            let wasm = compile_program(&sources)?;
            fs::write(output, wasm)?;
            Ok(())
        }
        Commands::Check { input_files } => {
            let sources: Vec<_> = input_files
                .into_iter()
                .map(|input_file| {
                    let source = fs::read_to_string(&input_file)?;
                    Ok((input_file, source))
                })
                .collect::<Result<Vec<_>, io::Error>>()?;
            frontend::check_program(&sources)?;
            Ok(())
        }
        Commands::Index { input_files } => {
            let sources: Vec<_> = input_files
                .into_iter()
                .map(|input_file| {
                    let source = fs::read_to_string(&input_file)?;
                    Ok((input_file, source))
                })
                .collect::<Result<Vec<_>, io::Error>>()?;
            write_index(&sources)?;
            Ok(())
        }
        Commands::LanguageServer { .. } => {
            // start_language_server()
            todo!();
        }
        Commands::Run { input_files } => {
            let sources: Vec<_> = input_files
                .into_iter()
                .map(|input_file| {
                    let source = fs::read_to_string(&input_file)?;
                    Ok((input_file, source))
                })
                .collect::<Result<Vec<_>, io::Error>>()?;
            let wasm = compile_program(&sources)?;
            let engine = Engine::new(Config::new().wasm_function_references(true).wasm_gc(true))?;
            let mut store = Store::new(&engine, ());
            let module = Module::new(&engine, &wasm)?;
            // TODO add log import
            let instance = Instance::new(&mut store, &module, &[])?;
            let main_func = instance.get_typed_func::<(), i32>(&mut store, "main")?;
            let result = main_func.call(&mut store, ())?;
            println!("Result: {:?}", result);
            Ok(())
        }
    }
}
