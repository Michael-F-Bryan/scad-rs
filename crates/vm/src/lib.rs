use std::{
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use scad_bytecode::{Chunk, Constant, Instruction, Program};

#[derive(Debug)]
pub struct VirtualMachine {
    program: Program,
    stack: Vec<Value>,
}

impl VirtualMachine {
    pub fn load(program: Program) -> Self {
        VirtualMachine {
            program,
            stack: Vec::new(),
        }
    }

    pub fn run(&mut self, mut cb: impl Callbacks) -> Result<(), RuntimeError> {
        let current_chunk = &self.program.chunk;
        let mut ip = 0;

        loop {
            let instruction = current_chunk.instructions[ip];
            cb.before_execute(current_chunk, ip, instruction, &self.stack);

            ip += 1;
            match instruction {
                scad_bytecode::Instruction::Constant(ix) => {
                    let constant = &current_chunk.constants[ix as usize];
                    self.stack.push(constant.into());
                }
                scad_bytecode::Instruction::Negate => {
                    let value = self.stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                    self.stack.push(value.negate()?);
                }
                scad_bytecode::Instruction::Return => {
                    let ret = self.stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                    println!("Returned {ret:?}");
                    return Ok(());
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    StackUnderflow,
    InvalidOperation {
        operation: &'static str,
        type_name: &'static str,
    },
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            RuntimeError::StackUnderflow => "Stack Underflow".fmt(f),
            RuntimeError::InvalidOperation {
                operation,
                type_name,
            } => write!(f, "Unable to {operation} a \"{type_name}\""),
        }
    }
}

impl std::error::Error for RuntimeError {}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Undef,
    Number(f64),
    Boolean(bool),
    String(Arc<str>),
}

impl Value {
    pub const fn type_name(&self) -> &'static str {
        match self {
            Value::Undef => "undef",
            Value::Number(_) => "number",
            Value::Boolean(_) => "bool",
            Value::String(_) => "string",
        }
    }

    pub fn negate(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            other => Err(RuntimeError::InvalidOperation {
                operation: "negate",
                type_name: other.type_name(),
            }),
        }
    }
}

impl From<&'_ Constant> for Value {
    fn from(c: &'_ Constant) -> Self {
        match c {
            Constant::Number(n) => Value::Number(n.raw()),
            Constant::String(s) => Value::String(Arc::clone(s)),
        }
    }
}

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
        _stack: &[Value],
    ) {
    }
}
