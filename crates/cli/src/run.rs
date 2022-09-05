use std::{fs::File, path::PathBuf};

use anyhow::Context;
use clap::Parser;
use scad_bytecode::{Disassembler, Program};
use scad_runtime::{Callbacks, Stack, VirtualMachine};
use semver::{Comparator, Version};

#[derive(Debug, Parser)]
pub struct Run {
    /// Run the VM in debug mode.
    #[clap(short, long)]
    debug: bool,
    /// The bytecode version for the provided file.
    #[clap(long, hide = true)]
    bytecode_version: Option<String>,
    #[clap(parse(from_os_str))]
    filename: PathBuf,
}

impl Run {
    pub fn execute(self) -> Result<(), anyhow::Error> {
        let Run {
            bytecode_version,
            filename,
            debug,
        } = self;

        if let Some(min_bytecode_version) = bytecode_version {
            bytecode_version_check(&min_bytecode_version)?;
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
            // TODO: print a backtrace pointing to the invalid line.
            return Err(e.into());
        }

        Ok(())
    }
}

/// Check that [`scad_bytecode::VERSION`] is compatible with the requested
/// bytecode version.
fn bytecode_version_check(min_bytecode_version: &str) -> Result<(), anyhow::Error> {
    let constraint: Comparator = min_bytecode_version.parse().context(
        "Unable to parse the min bytecode version as a version constraint (e.g. \"^0.1.0\")",
    )?;

    let builtin_bytecode_version: Version = scad_bytecode::VERSION.parse().expect("Always valid");

    if !constraint.matches(&builtin_bytecode_version) {
        anyhow::bail!("The requested bytecode version isn't compatible with this program ({builtin_bytecode_version} doesn't satisfy {constraint}");
    }

    Ok(())
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
