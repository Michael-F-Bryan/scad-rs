//! The bytecode format for a `scad` program.

use std::io::{Read, Write};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
