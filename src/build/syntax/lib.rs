#[derive(Debug, Clone)]
pub enum LibSrc {
    Simple(String),
    Git {
        url: String, 
        commit: Option<String>,
        branch: Option<String>,
    },
    Path(String),
}