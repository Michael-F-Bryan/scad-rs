//! The bytecode format for a `scad` program.

mod disassemble;

pub use crate::disassemble::Disassembler;

use std::{
    fmt::{self, Display, Formatter},
    io::{Read, Write},
    sync::Arc,
};

use noisy_float::types::R64;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Program {
    pub chunk: Chunk,
}

impl Program {
    /// Deserialize a [`Program`] from its on-disk format.
    pub fn deserialize(mut reader: impl Read) -> Result<Self, bincode::Error> {
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

    /// Serialize the [`Program`] into its on-disk format.
    pub fn serialize(&self, mut w: impl Write) -> Result<(), bincode::Error> {
        writeln!(w, "#!/bin env scad run --requires-at-least={VERSION}")?;
        w.write_all(b"\0")?;

        bincode::serialize_into(w, self)
    }
}

/// A single instruction executed by the virtual machine.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Instruction {
    /// Load a constant.
    Constant(u8),
    /// Negate the value at the top of the stack.
    Negate,
    /// Return from the current function.
    Return,
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
    String(Arc<str>),
}

impl Constant {
    pub fn number(double: f64) -> Self {
        Constant::Number(R64::new(double))
    }

    pub fn string(s: impl Into<Arc<str>>) -> Self {
        Constant::String(s.into())
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Number(n) => n.fmt(f),
            Constant::String(s) => s.fmt(f),
        }
    }
}
