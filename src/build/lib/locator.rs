use async_std::path::PathBuf;
use git2::{build::RepoBuilder, Repository};
use url::Url;

use crate::{build::syntax::info::LibSrc, diagnostic::{Diagnostic, Reporter}, span::{Span, Spanned}, spanned_error};

pub async fn locate_library(src: Spanned<LibSrc>, reporter: &Reporter) -> Result<PathBuf, ()> {
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
            match clone(span.clone(), Spanned::new(url, span), None, None).await {
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
            match clone(span, url, commit, branch).await {
                Ok(path) => Ok(path),
                Err(err) => {
                    reporter.report(err).await;
                    Err(())
                }
            }
        }
    }

}

async fn clone(span: Span, url: Spanned<Url>, commit_hash: Option<Spanned<String>>, branch: Option<Spanned<String>>) -> Result<PathBuf, Diagnostic> {
    let host = match url.host_str() {
        Some(host) => host,
        None => return Err(spanned_error!(url.into_span(), "cannot-be-a-base URLs are not allowed")),
    };

    let path_segments = match url.path_segments() {
        Some(segments) => segments,
        None => return Err(spanned_error!(url.into_span(), "cannot-be-a-base URLs are not allowed")),
    };

    let warp_home = match home::home_dir() {
        Some(dir) => dir.join(".warp"),
        None => return Err(spanned_error!(span, "failed to fetch home directory while cloning git repository")),
    };

    let mut repo_dir: PathBuf = warp_home.join("git").join(host).into();
    repo_dir.extend(path_segments);
    
    if repo_dir.exists().await {
        match async_std::fs::remove_dir_all(&repo_dir).await {
            Ok(_) => {}
            Err(err) => return Err(spanned_error!(span, "failed to remove existing cache: {err}"))
        }
    }

    let mut builder = RepoBuilder::new();
    if let Some(branch) = branch {
        builder.branch(&branch);
    }

    let repo = match builder.clone(&url.as_str(), repo_dir.as_path().into()) {
        Ok(repo) => repo,
        Err(err) => return Err(spanned_error!(span, "unable to clone git repository: {err}"))
    };

    if let Some(hash) = commit_hash {

        let commit = match repo.find_commit_by_prefix(&hash) {
            Ok(commit) => commit,
            Err(err) => return Err(spanned_error!(hash.into_span(), "unable to find commit: {err}")),
        };

        match repo.checkout_tree(commit.as_object(), None) {
            Ok(_) => {},
            Err(err) => return Err(spanned_error!(hash.into_span(), "failed to checkout to commit: {err}")),
        }
    }

    Ok(repo_dir)
}
