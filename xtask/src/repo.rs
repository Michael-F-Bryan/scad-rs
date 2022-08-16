use std::{
    fmt::{self, Formatter},
    path::{Path, PathBuf},
    process::{Command, ExitStatus, Output, Stdio},
};

use anyhow::{Context, Error};
use walkdir::WalkDir;

#[derive(Debug)]
pub struct Fixtures {
    fixtures_dir: PathBuf,
}

impl Fixtures {
    pub fn new(fixtures_dir: PathBuf) -> Self {
        Fixtures { fixtures_dir }
    }

    pub fn checkout() -> Result<Self, Error> {
        let crate_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let openscad_dir = crate_dir.join("vendor").join("openscad");

        if !openscad_dir.join("README.md").exists() {
            let mut cmd = Command::new("git");

            cmd.arg("submodule")
                .arg("update")
                .arg("--init")
                .arg("--recursive")
                .stderr(Stdio::piped())
                .stdout(Stdio::piped());

            let output = cmd.output().context("Unable to invoke `git`")?;

            if !output.status.success() {
                let err = Error::new(CommandFailed::new(&cmd, output));
                return Err(err.context("Unable to update the vendor/openscad submodule"));
            }
        }

        Ok(Fixtures::new(openscad_dir))
    }

    /// Find all `*.scad` files.
    pub fn scad_files(&self) -> impl Iterator<Item = PathBuf> {
        WalkDir::new(&self.fixtures_dir)
            .into_iter()
            .filter_map(|entry| entry.ok())
            .filter(|e| e.file_type().is_file() && e.path().extension() == Some("scad".as_ref()))
            .map(|entry| entry.into_path())
    }
}

#[derive(Debug, Clone)]
struct CommandFailed {
    command: String,
    status: ExitStatus,
    stdout: String,
    stderr: String,
}

impl CommandFailed {
    fn new(cmd: &Command, output: Output) -> Self {
        let Output {
            status,
            stdout,
            stderr,
        } = output;
        let command = format!("{cmd:?}");

        CommandFailed {
            command,
            status,
            stdout: String::from_utf8_lossy(&stdout).into_owned(),
            stderr: String::from_utf8_lossy(&stderr).into_owned(),
        }
    }
}

impl fmt::Display for CommandFailed {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let CommandFailed {
            command,
            status,
            stdout,
            stderr,
        } = self;
        let exit_code = status.code().unwrap_or(1);

        write!(
            f,
            "the command `{command}` failed with exit code {exit_code}"
        )?;

        if !stdout.is_empty() {
            writeln!(f)?;
            writeln!(f, "Stdout:")?;
            writeln!(f, "{stdout}")?;
        }

        if !stderr.is_empty() {
            writeln!(f)?;
            writeln!(f, "Stderr:")?;
            writeln!(f, "{stderr}")?;
        }

        Ok(())
    }
}

impl std::error::Error for CommandFailed {}
