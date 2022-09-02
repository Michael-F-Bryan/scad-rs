use std::{fs::File, path::PathBuf};

use anyhow::Context;
use clap::Parser;
use scad_bytecode::{Disassembler, Program};
use scad_runtime::{Callbacks, Stack, VirtualMachine};

#[derive(Debug, Parser)]
pub struct Run {
    /// Run the VM in debug mode.
    #[clap(short, long)]
    debug: bool,
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
            debug,
        } = self;

        if let Some(min_bytecode_version) = requires_at_least {
            assert_eq!(scad_bytecode::VERSION, min_bytecode_version);
        }

        let f = File::open(&filename)
            .with_context(|| format!("Unable to open \"{}\" for reading", filename.display()))?;

        let program = Program::deserialize(f).context("Unable to load the bytecode")?;
        let mut vm = VirtualMachine::load(program);

        let result = if debug {
            vm.run(DebugCallbacks::default())
        } else {
            vm.run(ProductionCallbacks)
        };

        if let Err(e) = result {
            todo!("Print a backtrace for {e}");
        }

        Ok(())
    }
}

#[derive(Debug, Default)]
struct DebugCallbacks {
    dis: Disassembler,
}

impl Callbacks for DebugCallbacks {
    fn before_execute(
        &mut self,
        current_chunk: &scad_bytecode::Chunk,
        instruction_pointer: usize,
        instruction: scad_bytecode::Instruction,
        _stack: &Stack,
    ) {
        let line_number = current_chunk.line_numbers[instruction_pointer];
        self.dis
            .instruction(current_chunk, instruction, line_number);
        let formatted = self.dis.finish();
        eprint!("{formatted}");
    }
}

struct ProductionCallbacks;

impl Callbacks for ProductionCallbacks {}
