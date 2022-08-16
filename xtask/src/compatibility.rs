use std::{
    fmt::{Debug, Display},
    panic::AssertUnwindSafe,
    path::{Path, PathBuf},
};

use anyhow::{Context, Error};
use rayon::prelude::*;
use scad_syntax::{ast::Package, ParseError};
use tracing::callsite;

use crate::Fixtures;

const SKIP_TOKENIZING: &[&str] = &[
    // "2D/issues/polyset-reduce-crash.scad",
    // unterminated tokens
    "issues/issue1890-comment.scad",
    "issues/issue1890-string.scad",
    "issues/issue1890-include.scad",
    "issues/issue1890-use.scad",
    // Not utf8
    "misc/nbsp-latin1-test.scad",
    "misc/ord-tests.scad",
];

#[derive(Debug, clap::Parser)]
pub struct Compatibility {
    test_folder: Option<PathBuf>,
    /// Ensure that there are no failing tests.
    #[clap(short, long)]
    check: bool,
    #[clap(long)]
    single_threaded: bool,
}

impl Compatibility {
    #[tracing::instrument(skip_all)]
    pub fn generate_compatibility_report(self) -> Result<(), Error> {
        let Compatibility {
            test_folder,
            check,
            single_threaded,
        } = self;
        tracing::info!("Generating the compatibility report");

        if single_threaded {
            std::env::set_var("RAYON_NUM_THREADS", "1");
        }

        let repo = match test_folder {
            Some(f) => Fixtures::new(f),
            None => Fixtures::checkout().context("Unable to load the test directory")?,
        };

        let project_root = crate::project_root();

        let cb = Callbacks {
            out_dir: project_root.join("target").join("integration-tests"),
            skip_tokenizing: SKIP_TOKENIZING,
        };

        let mut test_files = Vec::new();

        for path in repo.scad_files() {
            let contents = std::fs::read(&path)
                .with_context(|| format!("Unable to read \"{}\"", path.display()))?;

            match String::from_utf8(contents) {
                Ok(contents) => test_files.push(Input { path, contents }),
                Err(_) => {
                    tracing::debug!(path=%path.display(), "Skipping non-UTF8 input");
                }
            }
        }

        tracing::debug!(count = test_files.len(), "Loaded Fixtures");

        let report = CompatibilityReport {
            tokenize: check_inputs(
                &cb,
                &test_files,
                "tokenize",
                |cb, p| cb.should_tokenize(p),
                |i| scad_syntax::tokenize(&i.contents).collect(),
                |cb, name, tokens| cb.on_tokenized(name, tokens),
            ),
            parse: check_inputs(
                &cb,
                &test_files,
                "parse",
                |cb, p| cb.should_tokenize(p),
                |i| {
                    let tokens = scad_syntax::tokenize(&i.contents);
                    scad_syntax::parse(tokens)
                },
                |cb, name, (pkg, errors)| cb.on_parse(name, pkg, errors),
            ),
            type_check: Vec::new(),
            run: Vec::new(),
        };

        // tracing::info!(?report);

        if check && report.contains_failures() {
            Err(Error::msg("One or more failures occurred"))
        } else {
            Ok(())
        }
    }
}

fn check_inputs<'input, T: 'input>(
    cb: &Callbacks,
    inputs: &'input [Input],
    phase: &str,
    filter: impl Fn(&Callbacks, &Path) -> bool + Send + Sync,
    execute: impl Fn(&'input Input) -> T + Send + Sync,
    consume: impl Fn(&Callbacks, &str, T) -> Outcome + Send + Sync,
) -> Vec<Report> {
    inputs
        .par_iter()
        .map(move |input| {
            let _span =
                tracing::debug_span!("", phase = %phase, path = %input.path.display()).entered();
            let path = input.path.clone();

            if !filter(cb, &input.path) {
                tracing::debug!("Skipped");
                return Report {
                    path,
                    outcome: Outcome::Skipped,
                };
            }

            let result = match std::panic::catch_unwind(AssertUnwindSafe(|| execute(input))) {
                Ok(value) => value,
                Err(e) => {
                    let msg = if let Some(s) = e.downcast_ref::<&str>() {
                        s
                    } else if let Some(s) = e.downcast_ref::<String>() {
                        s.as_str()
                    } else {
                        "<unknown-panic>"
                    };
                    let error = anyhow::anyhow!(
                        "A panic occurred while processing \"{}\": {msg}",
                        input.path.display()
                    );
                    return Report {
                        path,
                        outcome: Outcome::Fail(error),
                    };
                }
            };

            Report {
                path,
                outcome: consume(cb, input.name(), result),
            }
        })
        .collect()
}

#[derive(Debug, Clone)]
struct Input {
    path: PathBuf,
    contents: String,
}

impl Input {
    fn name(&self) -> &str {
        self.path.file_stem().unwrap().to_str().unwrap()
    }
}

#[derive(Debug)]
struct CompatibilityReport {
    tokenize: Vec<Report>,
    parse: Vec<Report>,
    type_check: Vec<Report>,
    run: Vec<Report>,
}

impl CompatibilityReport {
    fn failures(&self) -> impl Iterator<Item = &'_ Report> + '_ {
        let CompatibilityReport {
            tokenize,
            parse,
            type_check,
            run,
        } = self;
        tokenize
            .iter()
            .chain(parse)
            .chain(type_check)
            .chain(run)
            .filter(|r| matches!(r.outcome, Outcome::Fail(_)))
    }

    fn contains_failures(&self) -> bool {
        self.failures().count() > 0
    }
}

#[derive(Debug)]
struct Report {
    path: PathBuf,
    outcome: Outcome,
}

#[derive(Debug, Clone)]
struct Callbacks {
    out_dir: PathBuf,
    skip_tokenizing: &'static [&'static str],
}

impl Callbacks {
    fn save(&self, phase: &str, fixture: &str, value: impl Debug) {
        if let Err(e) = self._save(phase, fixture, value) {
            tracing::warn!(phase, fixture, error = &*e, "Save failed");
        }
    }

    fn _save(&self, phase: &str, fixture: &str, value: impl Debug) -> Result<(), Error> {
        let dir = self.out_dir.join(phase);
        std::fs::create_dir_all(&dir)
            .with_context(|| format!("Unable create the \"{}\" directory", dir.display()))?;

        let value = format!("{value:#?}");
        let dest = dir.join(fixture).with_extension("out");
        std::fs::write(&dest, value.as_bytes())
            .with_context(|| format!("Unable save to \"{}\"", dest.display()))?;

        Ok(())
    }

    fn should_tokenize(&self, path: &Path) -> bool {
        self.skip_tokenizing
            .into_iter()
            .all(|suffix| !path.ends_with(suffix))
    }

    fn on_tokenized(&self, name: &str, tokens: Vec<(scad_syntax::SyntaxKind, &str)>) -> Outcome {
        self.save("tokenize", name, &tokens);

        let mut line_number = 1;

        for (kind, text) in tokens {
            if kind == scad_syntax::SyntaxKind::ERROR {
                return Outcome::Fail(anyhow::anyhow!(
                    "Invalid token on line {line_number}: {text:?}"
                ));
            }

            line_number += text.chars().filter(|&c| c == '\n').count();
        }

        Outcome::Pass
    }

    fn should_parse(&self, path: &Path) -> bool {
        self.should_tokenize(path)
    }

    fn on_parse(&self, name: &str, ast: Package, errors: Vec<ParseError>) -> Outcome {
        self.save("parse", name, (&ast, &errors));

        if !errors.is_empty() {
            return Outcome::Fail(anyhow::anyhow!("{errors:?}"));
        }

        Outcome::Pass
    }

    fn _should_type_check(&self, path: &Path) -> bool {
        self.should_parse(path) && false
    }

    fn _should_run(&self, path: &Path) -> bool {
        self._should_type_check(path) && false
    }
}

#[derive(Debug)]
enum Outcome {
    Pass,
    Fail(Error),
    Skipped,
}

fn tokenize_known_files(repo: &Fixtures) {
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

    for filename in repo.scad_files() {
        if ignored.iter().any(|suffix| filename.ends_with(suffix)) {
            continue;
        }

        println!("Reading \"{}\"", filename.display());

        let text = std::fs::read_to_string(&filename).unwrap();
        let tokens = scad_syntax::tokenize(&text);

        let mut line_number = 1;

        for (kind, text) in tokens {
            if kind == scad_syntax::SyntaxKind::ERROR {
                panic!("Invalid token on line {line_number}: {text:?}");
            }

            line_number += text.chars().filter(|&c| c == '\n').count();
        }
    }
}
