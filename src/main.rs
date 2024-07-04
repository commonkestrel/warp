mod build;
mod diagnostic;
mod span;

use std::process::ExitCode;

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
        input: clio::Input,
        #[arg(short, long)]
        output: clio::Output,
    },
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli.command {
        Command::Build { input, output } => {
            build::build(input, output)
        }
    }
}
