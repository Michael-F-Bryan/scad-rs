//! Disassembly routines for the bytecode format.

use crate::{Chunk, Instruction, Program};

/// An indent-aware pretty-printer.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Disassembler {
    buffer: String,
    indent_level: usize,
    previous_line_number: Option<u16>,
}

impl Disassembler {
    const INDENT: &'static str = "  ";

    pub fn new() -> Self {
        Disassembler::default()
    }

    /// Finish writing to this [`Disassembler`], returning the text generated
    /// up to this point and clearing the buffer for reuse.
    pub fn finish(&mut self) -> String {
        let formatted = self.buffer.clone();
        self.buffer.clear();
        formatted
    }

    pub fn program(&mut self, p: &Program) {
        let Program { chunk } = p;
        self.chunk(chunk);
    }

    pub fn chunk(&mut self, c: &Chunk) {
        self.previous_line_number = None;

        for (i, &inst) in c.instructions.iter().enumerate() {
            let line_number = c.line_numbers[i];
            self.instruction(c, inst, line_number);

            self.previous_line_number = Some(line_number);
        }
    }

    pub fn instruction(&mut self, c: &Chunk, inst: Instruction, line_number: u16) {
        if self.previous_line_number == Some(line_number) {
            write!(self, "   |");
        } else {
            write!(self, "{line_number:>3}|");
        }
        write!(self, "  ");
        match inst {
            Instruction::Constant(ix) => {
                let constant = &c.constants[ix as usize];
                writeln!(self, "load_constant {constant}");
            }
            Instruction::Negate => {
                writeln!(self, "negate");
            }
            Instruction::Not => {
                writeln!(self, "not");
            }
            Instruction::Return => writeln!(self, "ret"),
            Instruction::Add => writeln!(self, "add"),
            Instruction::Sub => writeln!(self, "sub"),
            Instruction::Mul => writeln!(self, "mul"),
            Instruction::Div => writeln!(self, "div"),
            Instruction::Undef => writeln!(self, "undef"),
            Instruction::True => writeln!(self, "true"),
            Instruction::False => writeln!(self, "false"),
            Instruction::Pop => writeln!(self, "pop"),
            Instruction::DefineGlobal(ix) => {
                let variable_name = &c.constants[ix as usize];
                writeln!(self, "define_global {variable_name}");
            }
            Instruction::LookupVariable(ix) => {
                let variable_name = &c.constants[ix as usize];
                writeln!(self, "lookup {variable_name}");
            }
            Instruction::Call(1) => {
                writeln!(self, "call (1 arg)");
            }
            Instruction::Call(arg_count) => {
                writeln!(self, "call ({arg_count} args)");
            }
        }
    }

    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) {
        let temp = args.to_string();

        if temp.trim_end().contains('\n') {
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
            constants: vec![Constant::string("Hello, World!"), Constant::number(42.0)],
            instructions: vec![
                Instruction::Constant(0),
                Instruction::Constant(1),
                Instruction::Negate,
                Instruction::Return,
            ],
            line_numbers: vec![0, 1, 2, 2],
        };
        let mut dis = Disassembler::default();
        let expected =
            "  0|  load_constant Hello, World!\n  1|  load_constant 42\n  2|  negate\n   |  ret\n";

        dis.chunk(&c);

        let disassembled = dis.finish();
        println!("{disassembled}");
        assert_eq!(disassembled, expected);
    }
}
