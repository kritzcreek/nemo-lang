use std::{error::Error, fs, path::PathBuf, process};

use clap::{Parser, Subcommand};
use frontend::{compile_program, render_errors};
use language_server::start_language_server;

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
    #[command(arg_required_else_help = true)]
    Compile {
        /// The *.nemo file to check
        input_file: PathBuf,
        /// When provided generates the Wasm with the given filename
        #[arg(long)]
        output: Option<PathBuf>,
    },
    /// Checks Nemo programs and reports any errors. Does not generate Wasm.
    #[command(arg_required_else_help = true)]
    Check {
        /// The *.nemo file to check
        input_file: PathBuf,
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
        Commands::Compile { input_file, output } => {
            let source = fs::read_to_string(&input_file)?;
            let (name_map, compiled) = compile_program(&source);
            match compiled {
                Ok(bytes) => {
                    fs::write(
                        output.unwrap_or_else(|| input_file.with_extension("wasm")),
                        bytes,
                    )?;
                    Ok(())
                }
                Err(e) => {
                    eprint!("{}", render_errors(&e, &source, &name_map));
                    process::exit(1)
                }
            }
        }
        Commands::Check { input_file } => {
            let source = fs::read_to_string(input_file)?;
            frontend::check_program(&source);
            Ok(())
        }
        Commands::LanguageServer { .. } => start_language_server(),
    }
}
