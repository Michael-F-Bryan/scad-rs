//! Disassembly routines for the bytecode format.

use crate::{Chunk, Program};

/// An indent-aware pretty-printer.
#[derive(Debug, Default, PartialEq)]
pub struct Disassembler {
    buffer: String,
    indent_level: usize,
}

impl Disassembler {
    const INDENT: &str = "  ";

    pub fn new() -> Self {
        Disassembler::default()
    }

    pub fn finish(self) -> String {
        let Disassembler { buffer, .. } = self;
        buffer
    }

    pub fn program(&mut self, p: &Program) {
        let Program { chunk } = p;
        self.chunk(chunk);
    }

    pub fn chunk(&mut self, c: &Chunk) {
        let mut previous_line_number: Option<u16> = None;

        for (i, inst) in c.instructions.iter().enumerate() {
            let line_number = c.line_numbers[i];

            if previous_line_number == Some(line_number) {
                let repeat_character = '|';
                write!(self, "{repeat_character:>3}");
            } else {
                write!(self, "{line_number:>3}");
            }

            write!(self, " ");

            match *inst {
                crate::Instruction::Constant(ix) => {
                    let constant = &c.constants[ix as usize];
                    writeln!(self, "constant {constant}");
                }
                crate::Instruction::Return => writeln!(self, "ret"),
            }

            previous_line_number = Some(line_number);
        }
    }

    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) {
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
            self.buffer.push_str(Disassembler::INDENT);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Constant, Instruction};

    #[test]
    fn disassemble_a_chunk() {
        let c = Chunk {
            instructions: vec![
                Instruction::Constant(0),
                Instruction::Constant(0),
                Instruction::Return,
            ],
            constants: vec![Constant::number(1.0)],
            line_numbers: vec![1, 2, 2],
        };
        let mut dis = Disassembler::default();
        let expected = "  1 constant 1\n  2 constant 1\n  | ret\n";

        dis.chunk(&c);

        assert_eq!(dis.finish(), expected);
    }
}
