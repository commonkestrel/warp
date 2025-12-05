use std::{collections::HashMap, io::Stdout, path::PathBuf};

use smol::fs::File;
use slotmap::SlotMap;

use crate::{build::{symbol_table::SymbolTable, syntax}};
use nurse::prelude::*;

use super::unresolved::{Item, ItemId, UnresolvedDb};

pub async fn resolve_lib(
    root_path: PathBuf,
    span: Span,
    libs: &mut HashMap<PathBuf, ItemId>,
    items: &mut SlotMap<ItemId, Item>,
    symbol_table: SymbolTable,
    reporter: &TerminalReporter<Stdout>,
) -> UnresolvedDb {
    let file_path = root_path.join("lib.warp");
    let file_name = file_path.to_string_lossy().replace('\\', "/");
    let file = match File::open(&file_path).await {
        Ok(file) => file,
        Err(err) => {
            reporter
                .report(error!(
                    span,
                    "unable to open input file: {}",
                    err
                ))
                .await;
            return UnresolvedDb::default();
        }
    };

    let lexed = match syntax::lex::lex(symbol_table, file_name, file, &reporter).await {
        Ok(lexed) => lexed,
        Err(_) => return UnresolvedDb::default(),
    };

    let namespace = match syntax::parse::parse(
        &lexed.stream,
        &reporter,
        &lexed.symbol_table,
        root_path,
    )
    .await
    {
        Ok(namespace) => namespace,
        Err(_) => return UnresolvedDb::default(),
    };

    Box::pin(UnresolvedDb::compile(
        namespace,
        lexed.symbol_table,
        libs,
        items,
        reporter,
    ))
    .await
}
