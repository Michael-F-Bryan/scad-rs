use std::{
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
};

use scad_syntax::SyntaxKind::ERROR;
use walkdir::WalkDir;

fn main() {
    let repo = Repo::checkout();

    tokenize_known_files(&repo);
}

fn tokenize_known_files(repo: &Repo) {
    let ignored = [
        "2D/issues/polyset-reduce-crash.scad",
        // unterminated tokens
        "issues/issue1890-comment.scad",
        "issues/issue1890-string.scad",
        "issues/issue1890-include.scad",
        "issues/issue1890-use.scad",
        // Not utf8
        "misc/nbsp-latin1-test.scad",
        "misc/ord-tests.scad",
    ];

    for filename in repo.all_tests() {
        if ignored.iter().any(|suffix| filename.ends_with(suffix)) {
            continue;
        }

        println!("Reading \"{}\"", filename.display());

        let text = std::fs::read_to_string(&filename).unwrap();
        let tokens = scad_syntax::tokenize(&text);

        let mut line_number = 1;

        for (kind, text) in tokens {
            if kind == ERROR {
                panic!("Invalid token on line {line_number}: {text:?}");
            }

            line_number += text.chars().filter(|&c| c == '\n').count();
        }
    }
}

#[derive(Debug)]
struct Repo {
    openscad_dir: PathBuf,
}

impl Repo {
    fn checkout() -> Self {
        let crate_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let openscad_dir = crate_dir.join("openscad");

        if !openscad_dir.join("README.md").exists() {
            let Output {
                status,
                stderr,
                stdout,
            } = Command::new("git")
                .arg("submodule")
                .arg("update")
                .arg("--init")
                .arg("--recursive")
                .stderr(Stdio::piped())
                .stdout(Stdio::piped())
                .output()
                .unwrap();

            if !status.success() {
                eprintln!("---- Stdout ----");
                eprintln!("{}", String::from_utf8_lossy(&stdout));
                eprintln!("---- Stder ----");
                eprintln!("{}", String::from_utf8_lossy(&stderr));

                panic!("Submodule update failed");
            }
        }

        Repo { openscad_dir }
    }

    fn test_dir(&self) -> PathBuf {
        self.openscad_dir.join("testdata").join("scad")
    }

    fn all_tests(&self) -> impl Iterator<Item = PathBuf> {
        WalkDir::new(self.test_dir())
            .into_iter()
            .filter_entry(|e| {
                e.file_type().is_dir() || e.path().extension() == Some("scad".as_ref())
            })
            .filter_map(|entry| entry.ok())
            .filter(|e| e.file_type().is_file())
            .map(|entry| entry.into_path())
    }
}
