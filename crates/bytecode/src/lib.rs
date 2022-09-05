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
    /// Define a global variable by name.
    DefineGlobal(u8),
    /// Lookup a variable by name.
    LookupVariable(u8),
    /// Load the constant, `undef`.
    Undef,
    /// Load the constant, `true`.
    True,
    /// Load the constant, `false`.
    False,
    /// Negate the value at the top of the stack.
    Negate,
    /// The logical not operator (often written as `!`).
    Not,
    /// Add the top two values on the stack.
    Add,
    /// Subtract the top two values on the stack.
    Sub,
    /// Multiply the top two values on the stack.
    Mul,
    /// Divide the top two values on the stack.
    Div,
    /// Return from the current function.
    Return,
    /// Remove a value from the top of the stack and forget it.
    Pop,
    /// Call the function at the top of the stack using.
    Call(u8),
    /// Push an empty list onto the stack.
    CreateList,
    /// Add the item on the top of the stack onto the list beneath it.
    AddToList,
    /// Pop a geometry object from the stack and send it to the output.
    SaveGeometry,
}

/// A set of instructions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct Chunk {
    pub constants: Vec<Constant>,
    pub instructions: Vec<Instruction>,
    /// The line number for each instruction in [`Chunk::instructions`].
    pub line_numbers: Vec<u16>,
}

impl Chunk {
    pub fn empty() -> Chunk {
        Chunk {
            constants: Vec::new(),
            instructions: Vec::new(),
            line_numbers: Vec::new(),
        }
    }

    pub fn push_instruction(&mut self, instruction: Instruction, line_number: u16) {
        self.instructions.push(instruction);
        self.line_numbers.push(line_number);
    }

    pub fn push_constant(&mut self, constant: impl Into<Constant>) -> u8 {
        self.constants.push(constant.into());
        (self.constants.len() - 1)
            .try_into()
            .expect("Chunks can't contain more than 255 constants")
    }
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

impl From<f64> for Constant {
    fn from(f: f64) -> Self {
        Constant::number(f)
    }
}

impl From<&'_ str> for Constant {
    fn from(s: &'_ str) -> Self {
        Constant::string(s)
    }
}

impl From<String> for Constant {
    fn from(s: String) -> Self {
        Constant::string(s)
    }
}

impl From<&'_ String> for Constant {
    fn from(s: &'_ String) -> Self {
        Constant::string(s.as_str())
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
