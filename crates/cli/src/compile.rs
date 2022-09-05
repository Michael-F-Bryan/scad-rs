use std::{fs::File, path::PathBuf};

use anyhow::{Context, Error};
use scad_compiler::{lowering::Lowering, parsing::Parsing, Database};

#[derive(Debug, clap::Parser)]
pub struct Compile {
    /// Where to save the compiled bytecode.
    #[clap(short, long, parse(from_os_str))]
    output: Option<PathBuf>,
    /// The file to compile.
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

impl Compile {
    pub fn execute(self) -> Result<(), Error> {
        let Compile { output, input } = self;

        let src = std::fs::read_to_string(&input)
            .with_context(|| format!("Unable to read \"{}\"", input.display()))?;

        let mut db = Database::default();
        db.set_src(src.into());

        let (compiled, diags) = db.compile();

        for diag in &diags {
            println!("{diag:?}");
        }

        let output = output.unwrap_or_else(|| input.with_extension("scadc"));
        let mut f = File::create(&output)
            .with_context(|| format!("Unable to open \"{}\" for writing", output.display()))?;

        compiled
            .serialize(&mut f)
            .context("Unable to save the compiled program")?;

        Ok(())
    }
}
