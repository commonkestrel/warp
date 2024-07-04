use std::{path::PathBuf, process::ExitCode};

use async_std::{ io::WriteExt, fs::File };
use clio::{Input, Output};

use crate::error;

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

pub async fn build(input: PathBuf, output: PathBuf) -> ExitCode {
    let file_name = input.to_string_lossy().into_owned();
    let file = match File::open(input).await {
        Ok(file) => file,
        Err(err) => {
            error!("unable to open input file: {}", err).emit().await;
            return ExitCode::FAILURE;
        }
    };

    match syntax::lex::lex(file_name, file).await {
        Ok(lexed) => {
            match File::create(output).await {
                Ok(mut file) => {
                    match write!(file, "{:#?}", lexed.stream).await {
                        Ok(_) => ExitCode::SUCCESS,
                        Err(err) => {
                            error!("unable to write to output file: {}", err).emit().await;
                            ExitCode::FAILURE
                        }
                    }
                }
                Err(err) => {
                    error!("unable to create output file: {}", err).emit().await;
                    ExitCode::FAILURE
                }
            }
        }
        Err(errors) => {
            for err in errors {
                err.emit().await;
            }

            ExitCode::FAILURE
        }
    }
}
