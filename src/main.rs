mod build;
use smol_macros::main;

use std::{path::PathBuf, process::ExitCode};

use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
    
}

#[derive(Debug, Subcommand)]
enum Command {
    Build {
        input: PathBuf,
        #[arg(short, long)]
        output: PathBuf,
        #[arg(short, long, action)]
        verbose: bool,
    },
}

main! {
    async fn main() -> ExitCode {
        let cli = Cli::parse();

        match cli.command {
            Command::Build { input, output, verbose } => build::build(input, output, verbose).await,
        }
    }
}
