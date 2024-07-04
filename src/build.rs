use std::process::ExitCode;

use clio::{Input, Output};

mod frontend {
    pub mod uncaught_error;
}

mod syntax {
    pub mod ast;
    pub mod lex;
    pub mod parse;
    pub mod token;
}

mod ascii;
mod symbol_table;

pub fn build(input: Input, output: Output) -> ExitCode {
    // let lexed = syntax::lex::lex(input.to_string(), input.);
    ExitCode::SUCCESS
}
