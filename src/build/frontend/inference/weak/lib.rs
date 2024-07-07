use std::env;

use async_std::{fs::File, path::PathBuf};

use crate::{build::{frontend::inference::Namespace, symbol_table::SymbolTable, syntax}, diagnostic::Reporter, error, span::Span, spanned_error};

pub async fn resolve_lib(root_path: PathBuf, span: Span, symbol_table: SymbolTable, reporter: &Reporter) -> Result<Namespace, ()> {
    let file_path = root_path.join("lib.warp");
    let file_name = file_path.to_string_lossy().replace('\\', "/");
    let file = match File::open(&file_path).await {
        Ok(file) => file,
        Err(err) => {
            reporter.report(spanned_error!(span, "unable to open input file: {}", err)).await;
            return Err(());
        }
    };

    let lexed = match syntax::lex::lex(symbol_table, file_name, file).await {
        Ok(lexed) => lexed,
        Err(errors) => {
            reporter.report_all(errors).await;

            return Err(());
        }
    };

    let namespace = match syntax::parse::parse(&lexed.stream, lexed.source, lexed.lookup, Reporter::new(), &lexed.symbol_table, root_path).await {
        Ok(namespace) => namespace,
        Err(reporter) => {
            reporter.emit_all().await;
            return Err(());
        }
    };

    todo!()
}
