mod value;

pub use crate::value::Value;

use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Neg, Sub},
};

use scad_bytecode::{Chunk, Instruction, Program};

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
                    let rhs = stack.pop()?;
                    let lhs = stack.pop()?;
                    stack.push(lhs.add(rhs)?);
                }
                scad_bytecode::Instruction::Sub => {
                    let rhs = stack.pop()?;
                    let lhs = stack.pop()?;
                    stack.push(lhs.sub(rhs)?);
                }
                scad_bytecode::Instruction::Mul => {
                    let rhs = stack.pop()?;
                    let lhs = stack.pop()?;
                    stack.push(lhs.mul(rhs)?);
                }
                scad_bytecode::Instruction::Div => {
                    let rhs = stack.pop()?;
                    let lhs = stack.pop()?;
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

/// The runtime stack.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Stack(Vec<Value>);

impl Stack {
    /// Get a reference to the value on the top of the stack.
    pub fn peek(&self) -> Option<&Value> {
        self.at(0)
    }

    /// Get a reference to the value `offset` from the top of the stack.
    pub fn at(&self, offset: usize) -> Option<&Value> {
        self.values().iter().nth_back(offset)
    }

    /// Pop a value from the top of the stack.
    pub fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.0.pop().ok_or(RuntimeError::StackUnderflow)
    }

    /// Push a value onto the stack.
    pub fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    /// All the values on the stack, ordered from oldest to most recent.
    pub fn values(&self) -> &[Value] {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    StackUnderflow,
    InvalidUnaryOperation {
        operation: &'static str,
        type_name: &'static str,
    },
    InvalidBinaryOperation {
        operation: &'static str,
        lhs: &'static str,
        rhs: &'static str,
    },
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            RuntimeError::StackUnderflow => "Stack Underflow".fmt(f),
            RuntimeError::InvalidUnaryOperation {
                operation,
                type_name,
            } => write!(f, "Unable to {operation} a {type_name}"),
            RuntimeError::InvalidBinaryOperation {
                operation,
                lhs,
                rhs,
            } => write!(f, "Unable to {operation} a {lhs} and a {rhs}"),
        }
    }
}

impl std::error::Error for RuntimeError {}

pub trait Callbacks {
    /// A callback that is fired immediately *before* executing the next
    /// instruction.
    ///
    /// At this point, the instruction pointer is still pointing at the current
    /// instruction.
    #[inline]
    fn before_execute(
        &mut self,
        _current_chunk: &Chunk,
        _instruction_pointer: usize,
        _instruction: Instruction,
        _stack: &Stack,
    ) {
    }
}
