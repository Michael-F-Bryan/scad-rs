//! Disassembly routines for the bytecode format.

use crate::Chunk;

/// Disassemble a single chunk.
pub fn chunk(w: &mut PrettyPrinter, c: &Chunk) {
    let mut previous_line_number: Option<u16> = None;

    for (i, inst) in c.instructions.iter().enumerate() {
        let line_number = c.line_numbers[i];

        if previous_line_number == Some(line_number) {
            let repeat_character = '|';
            write!(w, "{repeat_character:>3}");
        } else {
            write!(w, "{line_number:>3}");
        }

        write!(w, " ");

        match *inst {
            crate::Instruction::Constant(ix) => {
                let constant = &c.constants[ix as usize];
                writeln!(w, "constant {constant}");
            }
            crate::Instruction::Return => writeln!(w, "ret"),
        }

        previous_line_number = Some(line_number);
    }
}

/// An indent-aware pretty-printer.
#[derive(Debug, Default, PartialEq)]
pub struct PrettyPrinter {
    buffer: String,
    indent_level: usize,
}

impl PrettyPrinter {
    const INDENT: &str = "  ";

    pub fn new() -> Self {
        PrettyPrinter::default()
    }

    pub fn finish(self) -> String {
        let PrettyPrinter { buffer, .. } = self;
        buffer
    }

    pub fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) {
        let temp = args.to_string();

        if temp.trim_end().contains("\n") {
            for line in temp.lines() {
                self.write_indent();
                self.buffer.push_str(line);
                self.buffer.push('\n');
            }
        } else {
            self.buffer.push_str(&temp);
        }
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.buffer.push_str(PrettyPrinter::INDENT);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Constant, Instruction};
    use noisy_float::types::R64;

    #[test]
    fn disassemble_a_chunk() {
        let c = Chunk {
            instructions: vec![
                Instruction::Constant(0),
                Instruction::Constant(0),
                Instruction::Return,
            ],
            constants: vec![Constant::Number(R64::new(1.0))],
            line_numbers: vec![1, 2, 2],
        };
        let mut printer = PrettyPrinter::default();
        let expected = "  1 constant 1\n  2 constant 1\n  | ret\n";

        chunk(&mut printer, &c);

        assert_eq!(printer.buffer, expected);
    }
}
