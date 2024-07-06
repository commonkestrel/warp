use std::{env, path::PathBuf, process::ExitCode};

use async_std::{fs::File, io::WriteExt};
use clio::{Input, Output};
use syntax::{ast::Function, info::CompInfo, lex::Token, parse::{parse, Cursor}};

use crate::{diagnostic::Reporter, error, span::Spanned};

mod frontend {
    pub mod uncaught_error;
}

mod syntax {
    pub mod ast;
    pub mod info;
    pub mod lex;
    pub mod parse;
    pub mod token;
}

mod ascii;
mod symbol_table;

pub async fn build(input: PathBuf, output: PathBuf) -> ExitCode {
    let file_name = input.to_string_lossy().into_owned();
    let file = match File::open(&input).await {
        Ok(file) => file,
        Err(err) => {
            error!("unable to open input file: {}", err).emit().await;
            return ExitCode::FAILURE;
        }
    };

    let lexed = match syntax::lex::lex(file_name, file).await {
        Ok(lexed) => lexed,
        Err(errors) => {
            for err in errors {
                err.emit().await;
            }

            return ExitCode::FAILURE;
        }
    };

    let root_dir = match input.parent() {
        Some(parent) => {
            if parent.as_os_str().is_empty() || parent.starts_with(".") {
                match env::current_dir() {
                    Ok(cwd) => cwd,
                    Err(err) => {
                        error!("unable to get the current working directory: {err}").emit().await;
                        return ExitCode::FAILURE;
                    }
                }
            } else {
                parent.to_path_buf()
            }
        }
        None => match env::current_dir() {
            Ok(cwd) => cwd,
            Err(err) => {
                error!("unable to get the current working directory: {err}").emit().await;
                return ExitCode::FAILURE;
            }
        }
    };

    let namespace = match parse(&lexed.stream, lexed.source, lexed.lookup, Reporter::new(), root_dir.into()).await {
        Ok(namespace) => namespace,
        Err(reporter) => {
            reporter.emit_all().await;
            return ExitCode::FAILURE;
        }
    };

    match File::create(output).await {
        Ok(mut file) => match write!(file, "{:#?}", namespace).await {
            Ok(_) => return ExitCode::SUCCESS,
            Err(err) => {
                error!("unable to write to output file: {}", err)
                    .emit()
                    .await;
                return ExitCode::FAILURE;
            }
        },
        Err(err) => {
            error!("unable to create output file: {}", err).emit().await;
            return ExitCode::FAILURE;
        }
    }
}
