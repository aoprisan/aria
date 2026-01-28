mod commands;
mod errors;
mod repl;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "aria")]
#[command(author, version, about = "Aria compiler - a statically-typed language that compiles to WebAssembly")]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse and type-check a source file
    Check {
        /// Path to the Aria source file
        file: PathBuf,
    },
    /// Compile a source file to WebAssembly
    Build {
        /// Path to the Aria source file
        file: PathBuf,
        /// Output path for the WASM file (default: {stem}.wasm)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Compile and execute a source file
    Run {
        /// Path to the Aria source file
        file: PathBuf,
    },
    /// Start an interactive REPL
    Repl,
}

/// Run the CLI, returning an exit code (0 for success, non-zero for error).
pub fn run() -> Result<(), i32> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Check { file } => commands::check(&file),
        Commands::Build { file, output } => commands::build(&file, output.as_deref()),
        Commands::Run { file } => commands::run(&file),
        Commands::Repl => repl::run(),
    }
}
