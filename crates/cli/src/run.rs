use std::{fs::File, path::PathBuf};

use anyhow::Context;
use clap::Parser;
use scad_bytecode::Program;
use scad_vm::VirtualMachine;

#[derive(Debug, Parser)]
pub struct Run {
    /// The minimum bytecode version.
    #[clap(long, hide = true)]
    requires_at_least: Option<String>,
    #[clap(parse(from_os_str))]
    filename: PathBuf,
}

impl Run {
    pub fn execute(self) -> Result<(), anyhow::Error> {
        let Run {
            requires_at_least,
            filename,
        } = self;

        if let Some(min_bytecode_version) = requires_at_least {
            assert_eq!(scad_bytecode::VERSION, min_bytecode_version);
        }

        let f = File::open(&filename)
            .with_context(|| format!("Unable to open \"{}\" for reading", filename.display()))?;

        let program = Program::deserialize(f).context("Unable to load the bytecode")?;
        let mut vm = VirtualMachine::load(program);

        if let Err(e) = vm.run() {
            todo!("Print a backtrace for {e}");
        }

        Ok(())
    }
}
