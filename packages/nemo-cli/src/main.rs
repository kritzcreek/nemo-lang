use std::{error::Error, fs, path::PathBuf};

use clap::{Parser, Subcommand};
use nemo_backend::compile_program;
use nemo_frontend::{check_program, type_errors::render_ty_error};
use nemo_language_server::start_language_server;
use playground::run_playground;

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
        output_file: Option<PathBuf>,
    },
    /// Checks Nemo programs and reports any errors. Does not generate Wasm.
    #[command(arg_required_else_help = true)]
    Check {
        /// The *.nemo file to check
        input_file: PathBuf,
    },
    /// Runs the language server
    LanguageServer,
    /// Runs the Nemo playground where you can write Nemo programs
    /// that interact with the HTML5 canvas
    Playground,
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let args = Cli::parse();
    match args.command {
        Commands::Compile {
            input_file,
            output_file,
        } => {
            let source = fs::read_to_string(&input_file)?;
            let compiled = compile_program(&source);
            fs::write(
                output_file.unwrap_or_else(|| input_file.with_extension("wasm")),
                compiled,
            )?;
            Ok(())
        }
        Commands::Check { input_file } => {
            let source = fs::read_to_string(&input_file)?;
            match check_program(&source) {
                Ok(_) => Ok(()),
                Err(err) => {
                    eprintln!("{}", render_ty_error(&source, &err, true));
                    std::process::exit(1)
                }
            }
        }
        Commands::LanguageServer => start_language_server(),
        Commands::Playground => {
            run_playground();
            Ok(())
        }
    }
}
