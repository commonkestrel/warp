use std::{collections::HashMap, env, io::stdout, path::PathBuf, process::ExitCode};

use clio::{Input, Output};
use frontend::hir::{weak::unresolved::UnresolvedDb, Database};
use slotmap::SlotMap;
use smol::{fs::File, io::AsyncWriteExt, Unblock};
use symbol_table::SymbolTable;
use syntax::{
    ast::Function,
    info::CompInfo,
    lex::Token,
    parse::{parse, Cursor},
};

use nurse::prelude::*;

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

pub async fn build(input: PathBuf, output: PathBuf, verbose: bool) -> ExitCode {
    let filter = if verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Warn
    };

    let mut reporter = TerminalReporter::filtered(stdout(), filter);

    let file_name = input.to_string_lossy().replace('\\', "/");
    let file = match File::open(&input).await {
        Ok(file) => file,
        Err(err) => {
            let _ = reporter
                .emit(error!("unable to open input file: {}", err))
                .await;
            return ExitCode::FAILURE;
        }
    };

    reporter.report(debug!("loaded input file")).await;

    let symbol_table = SymbolTable::default();
    let lexed = match syntax::lex::lex(symbol_table, file_name, file, &reporter).await {
        Ok(lexed) => lexed,
        Err(_) => {
            let _ = reporter.emit_all().await;
            return ExitCode::FAILURE;   
        }
    };

    let root_dir = match input.parent() {
        Some(parent) => {
            if parent.as_os_str().is_empty() || parent.starts_with(".") {
                match env::current_dir() {
                    Ok(cwd) => cwd,
                    Err(err) => {
                        let _ = reporter
                            .emit(error!("unable to get the current working directory: {err}"))
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
                let _ = reporter
                    .emit(error!("unable to get the current working directory: {err}"))
                    .await;
                return ExitCode::FAILURE;
            }
        },
    };

    let namespace = match parse(
        &lexed.stream,
        &reporter,
        &lexed.symbol_table,
        root_dir
    )
    .await
    {
        Ok(namespace) => namespace,
        Err(_) => {
            let _ = reporter.emit_all().await;
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
        &reporter,
    )
    .await;

    match File::create(output).await {
        Ok(mut file) => match file.write_all(format!("{:#?}", db).as_bytes()).await {
            Ok(_) => return ExitCode::SUCCESS,
            Err(err) => {
                let _ = reporter
                    .emit(error!("unable to write to output file: {}", err))
                    .await;
                return ExitCode::FAILURE;
            }
        },
        Err(err) => {
            let _ = reporter
                .emit(error!("unable to create output file: {}", err))
                .await;
            return ExitCode::FAILURE;
        }
    }
}
