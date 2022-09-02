use std::ops::{Add, Div, Mul, Neg, Sub};

use scad_bytecode::Program;

use crate::{Callbacks, RuntimeError, Stack};

/// The OpenSCAD virtual machine.
#[derive(Debug)]
pub struct VirtualMachine {
    program: Program,
    stack: Stack,
}

impl VirtualMachine {
    pub fn load(program: Program) -> Self {
        VirtualMachine {
            program,
            stack: Stack::default(),
        }
    }

    pub fn run(&mut self, mut cb: impl Callbacks) -> Result<(), RuntimeError> {
        let VirtualMachine { program, stack } = self;
        let current_chunk = &program.chunk;
        let mut ip = 0;

        loop {
            let instruction = current_chunk.instructions[ip];
            cb.before_execute(current_chunk, ip, instruction, stack);

            ip += 1;
            match instruction {
                scad_bytecode::Instruction::Constant(ix) => {
                    let constant = &current_chunk.constants[ix as usize];
                    stack.push(constant.into());
                }
                scad_bytecode::Instruction::Negate => {
                    let value = stack.pop()?;
                    stack.push(value.neg()?);
                }
                scad_bytecode::Instruction::Add => {
                    let (lhs, rhs) = stack.pop2()?;
                    stack.push(lhs.add(rhs)?);
                }
                scad_bytecode::Instruction::Sub => {
                    let (lhs, rhs) = stack.pop2()?;
                    stack.push(lhs.sub(rhs)?);
                }
                scad_bytecode::Instruction::Mul => {
                    let (lhs, rhs) = stack.pop2()?;
                    stack.push(lhs.mul(rhs)?);
                }
                scad_bytecode::Instruction::Div => {
                    let (lhs, rhs) = stack.pop2()?;
                    stack.push(lhs.div(rhs)?);
                }
                scad_bytecode::Instruction::Return => {
                    let ret = stack.pop()?;
                    println!("Returned {ret:?}");
                    return Ok(());
                }
            }
        }
    }
}
