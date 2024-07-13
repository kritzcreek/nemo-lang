use backend::compile_program;
use clap::{Parser, Subcommand};
use language_server::start_language_server;
use std::{error::Error, fs, path::PathBuf};

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
        /// The *.nemo file to check
        input_file: PathBuf,
        /// When provided generates the Wasm with the given filename
        #[arg(long)]
        output: Option<PathBuf>,
    },
    /// Checks Nemo programs and reports any errors. Does not generate Wasm.
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
    tracing_subscriber::fmt::init();
    let args = Cli::parse();
    match args.command {
        Commands::Compile { input_file, output } => {
            let source = fs::read_to_string(&input_file)?;
            let wasm =
                tracing::info_span!("compile_program").in_scope(|| compile_program(&source))?;
            let output_file = output.unwrap_or_else(|| input_file.with_extension("wasm"));
            fs::write(output_file, wasm)?;
            Ok(())
        }
        Commands::Check { input_file } => {
            let source = fs::read_to_string(input_file)?;
            frontend::check_program(&source)?;
            Ok(())
        }
        Commands::LanguageServer { .. } => start_language_server(),
    }
}
