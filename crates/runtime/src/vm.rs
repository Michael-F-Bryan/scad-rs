use std::ops::{Add, Div, Mul, Neg, Not, Sub};

use scad_bytecode::{Chunk, Program};

use crate::{Callbacks, RuntimeError, Stack, Value};

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

            match evaluate(instruction, current_chunk, stack) {
                Ok(_) => {}
                Err(Break::Error(e)) => return Err(runtime_error(current_chunk, ip, e)),
                Err(Break::Return) => return Ok(()),
            }

            ip += 1;
        }
    }
}

#[cold]
fn runtime_error(current_chunk: &Chunk, ip: usize, cause: RuntimeError) -> RuntimeError {
    // TODO: generate a user-friendly stack trace and create a more useful error.
    let _line_number = &current_chunk.line_numbers[ip];
    cause
}

#[inline(always)]
fn evaluate(
    instruction: scad_bytecode::Instruction,
    current_chunk: &Chunk,
    stack: &mut Stack,
) -> Result<(), Break> {
    match instruction {
        scad_bytecode::Instruction::Constant(ix) => {
            let constant = &current_chunk.constants[ix as usize];
            stack.push(constant.into());
        }
        scad_bytecode::Instruction::Negate => {
            let value = stack.pop()?;
            stack.push(value.neg()?);
        }
        scad_bytecode::Instruction::Not => {
            let value = stack.pop()?;
            stack.push(value.not()?);
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
            return Err(Break::Return);
        }
        scad_bytecode::Instruction::Undef => stack.push(Value::Undef),
        scad_bytecode::Instruction::True => stack.push(Value::Boolean(true)),
        scad_bytecode::Instruction::False => stack.push(Value::Boolean(false)),
    }

    Ok(())
}

#[derive(Debug)]
enum Break {
    Error(RuntimeError),
    Return,
}

impl From<RuntimeError> for Break {
    fn from(r: RuntimeError) -> Self {
        Break::Error(r)
    }
}
