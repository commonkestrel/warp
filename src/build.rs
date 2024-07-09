use std::{collections::HashMap, env, path::PathBuf, process::ExitCode};

use async_std::{fs::File, io::WriteExt};
use clio::{Input, Output};
use frontend::hir::{weak::unresolved::UnresolvedDb, Database};
use slotmap::SlotMap;
use symbol_table::SymbolTable;
use syntax::{
    ast::Function,
    info::CompInfo,
    lex::Token,
    parse::{parse, Cursor},
};

use crate::{diagnostic::Reporter, error, span::Spanned};

mod frontend {
    pub mod hir;
    pub mod uncaught_error;
}

mod syntax {
    pub mod ast;
    pub mod info;
    pub mod lex;
    pub mod parse;
    pub mod token;
}

mod lib {
    pub mod locator;
}

mod ascii;
mod symbol_table;

pub async fn build(input: PathBuf, output: PathBuf) -> ExitCode {
    let file_name = input.to_string_lossy().replace('\\', "/");
    let file = match File::open(&input).await {
        Ok(file) => file,
        Err(err) => {
            error!("unable to open input file: {}", err).emit().await;
            return ExitCode::FAILURE;
        }
    };

    let symbol_table = SymbolTable::default();
    let lexed = match syntax::lex::lex(symbol_table, file_name, file).await {
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
                        error!("unable to get the current working directory: {err}")
                            .emit()
                            .await;
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
                error!("unable to get the current working directory: {err}")
                    .emit()
                    .await;
                return ExitCode::FAILURE;
            }
        },
    };

    let reporter = Reporter::new();
    let namespace = match parse(
        &lexed.stream,
        lexed.source,
        lexed.lookup,
        reporter.clone(),
        &lexed.symbol_table,
        root_dir.into(),
    )
    .await
    {
        Ok(namespace) => namespace,
        Err(reporter) => {
            reporter.emit_all().await;
            return ExitCode::FAILURE;
        }
    };

    let mut libs = HashMap::new();
    let mut items = SlotMap::with_key();
    let db = UnresolvedDb::compile(
        namespace,
        lexed.symbol_table,
        &mut libs,
        &mut items,
        reporter.clone(),
    )
    .await;

    match File::create(output).await {
        Ok(mut file) => match write!(file, "{:#?}", db).await {
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
