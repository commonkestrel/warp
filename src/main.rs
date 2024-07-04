mod build;
mod diagnostic;
mod span;

use std::{path::PathBuf, process::ExitCode};

use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
}

#[derive(Debug, Subcommand)]
enum Command {
    Build {
        input: PathBuf,
        #[arg(short, long)]
        output: PathBuf,
    },
}

#[async_std::main]
async fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli.command {
        Command::Build { input, output } => {
            build::build(input, output).await
        }
    }
}
