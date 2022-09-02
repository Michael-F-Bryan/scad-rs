//! The bytecode format for a `scad` program.

pub mod disassemble;

use std::{
    fmt::{self, Display, Formatter},
    io::{Read, Write},
};

use noisy_float::types::R64;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Program {}

impl Program {
    /// Serialize the [`Program`] into its on-disk format.
    pub fn serialize(&self, mut w: impl Write) -> Result<(), bincode::Error> {
        writeln!(w, "#!/bin env scadders --requires-at-least={VERSION}")?;
        w.write_all(b"\0")?;

        bincode::serialize_into(w, self)
    }

    /// Deserialize a [`Program`] from its on-disk format.
    pub fn deserialize(mut reader: impl Read) -> Result<(), bincode::Error> {
        // Skip everything up to and including the null terminator
        let mut buffer = [0];
        loop {
            reader.read_exact(&mut buffer)?;
            if buffer[0] == 0 {
                break;
            }
        }

        bincode::deserialize_from(reader)
    }
}

/// A single instruction executed by the virtual machine.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Instruction {
    /// Load a constant.
    Constant(u8),
    /// Return from the current function.
    Return,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Constant(ix) => write!(f, "constant ${ix}"),
            Instruction::Return => "ret".fmt(f),
        }
    }
}

/// A set of instructions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct Chunk {
    pub constants: Vec<Constant>,
    pub instructions: Vec<Instruction>,
    /// The line number for each instruction in [`Chunk::instructions`].
    pub line_numbers: Vec<u16>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Constant {
    Number(R64),
    String(String),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Number(n) => n.fmt(f),
            Constant::String(s) => s.fmt(f),
        }
    }
}
