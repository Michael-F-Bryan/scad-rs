use std::{fs::File, path::PathBuf};

use anyhow::Context;
use clap::Parser;
use scad_bytecode::{Disassembler, Program};

#[derive(Debug, Parser)]
pub struct Disassemble {
    #[clap(parse(from_os_str))]
    filename: PathBuf,
}

impl Disassemble {
    pub fn execute(self) -> Result<(), anyhow::Error> {
        let Disassemble { filename } = self;

        let f = File::open(&filename)
            .with_context(|| format!("Unable to open \"{}\" for reading", filename.display()))?;

        let program = Program::deserialize(f).context("Unable to load the bytecode")?;

        let mut dis = Disassembler::new();
        dis.program(&program);
        let disassembled = dis.finish();

        println!("{disassembled}");

        Ok(())
    }
}
