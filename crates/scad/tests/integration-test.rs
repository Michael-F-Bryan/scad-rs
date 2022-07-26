use std::{
    path::{Path, PathBuf},
    process::Command,
};

use once_cell::sync::Lazy;
use scad::syntax::SyntaxKind::ERROR;
use walkdir::WalkDir;

const OPENSCAD_REPO: &str = "https://github.com/openscad/openscad";
const OPENSCAD_TAG: &str = "openscad-2021.01";
static OPENSCAD: Lazy<Repo> = Lazy::new(Repo::checkout);

#[test]
fn tokenize_known_files() {
    let ignored = [
        "2D/issues/polyset-reduce-crash.scad",
        "issues/issue1890-comment.scad",
        "issues/issue1890-string.scad",
        "misc/nbsp-latin1-test.scad",
        "misc/ord-tests.scad",
        "templates/include-tests-template.scad",
        "templates/use-tests-template.scad",
    ];

    for filename in OPENSCAD.all_tests() {
        if ignored.iter().any(|suffix| filename.ends_with(suffix)) {
            continue;
        }

        println!("Reading \"{}\"", filename.display());

        let text = std::fs::read_to_string(&filename).unwrap();
        let tokens = scad::syntax::tokenize(&text);

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
    checkout_dir: PathBuf,
}

impl Repo {
    fn checkout() -> Self {
        let temp_dir = Path::new(env!("CARGO_TARGET_TMPDIR"));
        let checkout_dir = temp_dir.join("openscad");

        if !checkout_dir.exists() {
            let status = Command::new("git")
                .arg("clone")
                .arg(OPENSCAD_REPO)
                .arg(&checkout_dir)
                .arg("--quiet")
                .args(["--branch", OPENSCAD_TAG])
                .args(["--depth", "1"])
                .status()
                .unwrap();
            assert!(status.success(), "Checkout failed");
        }

        Repo { checkout_dir }
    }

    fn test_dir(&self) -> PathBuf {
        self.checkout_dir.join("testdata").join("scad")
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
