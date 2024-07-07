use async_std::path::PathBuf;

use crate::{build::syntax::info::LibSrc, diagnostic::{Diagnostic, Reporter}, span::Spanned, spanned_error};

pub async fn locate_library(src: Spanned<LibSrc>, reporter: Reporter) -> Result<PathBuf, ()> {
    let (source, span) = src.deconstruct();

    match source {
        LibSrc::Path(path) => {
            if !path.exists().await {
                reporter.report(spanned_error!(span, "path does not exist")).await;
                Err(())
            } else {
                Ok(path)
            }
        }
        LibSrc::Simple(url) => {
            match clone(Spanned::new(url, span), None, None).await {
                Ok(path) => Ok(path),
                Err(err) => {
                    reporter.report(err).await;
                    Err(())
                }
            }
        }
        LibSrc::Git {
            url, commit, branch
        } => {
            match clone(url, commit, branch).await {
                Ok(path) => Ok(path),
                Err(err) => {
                    reporter.report(err).await;
                    Err(())
                }
            }
        }
    }

}

async fn clone(url: Spanned<String>, commit: Option<Spanned<String>>, branch: Option<Spanned<String>>) -> Result<PathBuf, Diagnostic> {
    let warp_home: PathBuf = match home::home_dir() {
        Some(dir) => dir.join(".warp").into(),
        None => return Err(spanned_error!(url.into_span(), "failed to fetch home directory while cloning git repository")),
    };

    if !warp_home.exists().await {
        if let Err(err) = async_std::fs::create_dir(warp_home).await {
            return Err(spanned_error!(url.into_span(), "unable to create `.warp` home directory: {err}"));
        }
    }

    todo!()
}
