use std::{collections::HashMap, env, sync::Arc};

use async_std::{
    fs::File,
    path::{Path, PathBuf},
};
use slotmap::SlotMap;

use crate::{
    build::{symbol_table::SymbolTable, syntax},
    diagnostic::Reporter,
    error,
    span::Span,
    spanned_error,
};

use super::hir::UnresolvedDb;

pub async fn resolve_lib(
    root_path: PathBuf,
    span: &Span,
    libs: &mut HashMap<PathBuf, UnresolvedDb>,
    symbol_table: SymbolTable,
    reporter: &Reporter,
) -> UnresolvedDb {
    let file_path = root_path.join("lib.warp");
    let file_name = file_path.to_string_lossy().replace('\\', "/");
    let file = match File::open(&file_path).await {
        Ok(file) => file,
        Err(err) => {
            reporter
                .report(spanned_error!(
                    span.clone(),
                    "unable to open input file: {}",
                    err
                ))
                .await;
            return UnresolvedDb::default();
        }
    };

    let lexed = match syntax::lex::lex(symbol_table, file_name, file).await {
        Ok(lexed) => lexed,
        Err(errors) => {
            reporter.report_all(errors).await;

            return UnresolvedDb::default();
        }
    };

    let reporter = Reporter::new();
    let namespace = match syntax::parse::parse(
        &lexed.stream,
        lexed.source,
        lexed.lookup,
        reporter.clone(),
        &lexed.symbol_table,
        root_path,
    )
    .await
    {
        Ok(namespace) => namespace,
        Err(reporter) => {
            reporter.emit_all().await;
            return UnresolvedDb::default();
        }
    };

    let mut items = SlotMap::with_key();
    Box::pin(UnresolvedDb::compile(namespace, lexed.symbol_table, libs, &mut items, reporter)).await
}
