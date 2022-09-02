use std::fmt::{self, Display, Formatter};

use scad_bytecode::Program;

#[derive(Debug)]
pub struct VirtualMachine {
    program: Program,
}

impl VirtualMachine {
    pub fn load(program: Program) -> Self {
        VirtualMachine { program }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        let chunk = &self.program.chunk;
        let mut ip = 0;

        loop {
            let instruction = chunk.instructions[ip];
            ip += 1;

            match instruction {
                scad_bytecode::Instruction::Constant(ix) => {
                    let constant = &chunk.constants[ix as usize];
                    println!("{}", constant);
                }
                scad_bytecode::Instruction::Return => return Ok(()),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {}

impl Display for RuntimeError {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl std::error::Error for RuntimeError {}
