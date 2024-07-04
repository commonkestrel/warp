mod span;
mod diagnostic;
mod build;

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
    }
}

fn main() {
    println!("Hello, world!");
}