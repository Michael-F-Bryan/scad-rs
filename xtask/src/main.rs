mod compatibility;
mod repo;

use std::path::{Path, PathBuf};

pub use crate::repo::Fixtures;

use anyhow::Error;
use clap::Parser;

use tracing_subscriber::{fmt::format::FmtSpan, EnvFilter};

fn main() -> Result<(), Error> {
    if std::env::var_os("RUST_LOG").is_none() {
        std::env::set_var("RUST_LOG", "info");
    }

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_span_events(FmtSpan::CLOSE)
        .init();

    match Cmd::parse() {
        Cmd::Compatibility(c) => c.generate_compatibility_report(),
    }
}

#[derive(Debug, clap::Parser)]
enum Cmd {
    #[clap(aliases = &["c", "compat"])]
    Compatibility(compatibility::Compatibility),
}

pub(crate) fn project_root() -> PathBuf {
    let crate_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let project_root = crate_root.ancestors().nth(1).unwrap();
    assert!(project_root.join(".git").exists());
    project_root.to_path_buf()
}
