use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Neg, Not, Sub},
    sync::Arc,
};

use scad_bytecode::{Chunk, Constant, Instruction, Program};

use crate::{Callbacks, RuntimeError, Stack, Type, Value};

/// The OpenSCAD virtual machine.
#[derive(Debug)]
pub struct VirtualMachine {
    program: Program,
    stack: Stack,
    globals: HashMap<Arc<str>, Value>,
}

impl VirtualMachine {
    pub fn load(program: Program) -> Self {
        VirtualMachine {
            program,
            stack: Stack::default(),
            globals: crate::prelude(),
        }
    }

    pub fn run(&mut self, mut cb: impl Callbacks) -> Result<(), RuntimeError> {
        let VirtualMachine {
            program,
            stack,
            globals,
        } = self;
        let current_chunk = &program.chunk;
        let mut ip = 0;

        loop {
            let instruction = current_chunk.instructions[ip];
            cb.before_execute(current_chunk, ip, instruction, stack);

            match evaluate(instruction, current_chunk, stack, globals, &mut cb) {
                Ok(_) => {}
                Err(Break::Error(e)) => return Err(runtime_error(current_chunk, ip, e)),
                Err(Break::Return) => break,
            }

            ip += 1;
        }

        assert!(
            stack.values().is_empty(),
            "Exiting with a non-empty stack: {stack:?}"
        );

        if !stack.values().is_empty() {
            tracing::warn!("Exited with a non-empty stack");
        }

        Ok(())
    }
}

#[cold]
fn runtime_error(current_chunk: &Chunk, ip: usize, cause: RuntimeError) -> RuntimeError {
    // TODO: generate a user-friendly stack trace and create a more useful error.
    let _line_number = &current_chunk.line_numbers[ip];
    cause
}

#[inline]
fn evaluate(
    instruction: Instruction,
    current_chunk: &Chunk,
    stack: &mut Stack,
    globals: &mut HashMap<Arc<str>, Value>,
    callbacks: &mut impl Callbacks,
) -> Result<(), Break> {
    match instruction {
        Instruction::Constant(ix) => {
            let constant = &current_chunk.constants[ix as usize];
            stack.push(constant.into());
        }
        Instruction::Negate => {
            let value = stack.pop()?;
            stack.push(value.neg()?);
        }
        Instruction::Not => {
            let value = stack.pop()?;
            stack.push(value.not()?);
        }
        Instruction::Add => {
            let (lhs, rhs) = stack.pop2()?;
            stack.push(lhs.add(rhs)?);
        }
        Instruction::Sub => {
            let (lhs, rhs) = stack.pop2()?;
            stack.push(lhs.sub(rhs)?);
        }
        Instruction::Mul => {
            let (lhs, rhs) = stack.pop2()?;
            stack.push(lhs.mul(rhs)?);
        }
        Instruction::Div => {
            let (lhs, rhs) = stack.pop2()?;
            stack.push(lhs.div(rhs)?);
        }
        Instruction::Return => {
            let ret = stack.pop()?;
            println!("Returned {ret:?}");
            return Err(Break::Return);
        }
        Instruction::Undef => stack.push(Value::Undef),
        Instruction::True => stack.push(Value::Boolean(true)),
        Instruction::False => stack.push(Value::Boolean(false)),
        Instruction::Pop => {
            stack.pop()?;
        }
        Instruction::DefineGlobal(ix) => {
            let name = match &current_chunk.constants[ix as usize] {
                Constant::String(s) => Arc::clone(s),
                other => panic!("Attempted to do a variable definition with a non-string name ({other:?}). This is a bug."),
            };

            let value = stack.pop()?;
            globals.insert(name, value);
        }
        Instruction::LookupVariable(ix) => {
            let name = match &current_chunk.constants[ix as usize] {
                Constant::String(s) => s,
                other => panic!("Attempted to do a variable lookup with a non-string name ({other:?}). This is a bug."),
            };

            match globals.get(name) {
                Some(v) => stack.push(v.clone()),
                None => {
                    return Err(RuntimeError::UnknownVariable {
                        name: Arc::clone(name),
                    }
                    .into())
                }
            }
        }
        Instruction::Call(num_args) => {
            let function = stack.pop()?;
            let args = stack.pop_many(num_args as usize)?;

            let result = match function {
                Value::BuiltinFunction(f) => f.call(args)?,
                other => {
                    return Err(RuntimeError::NotCallable {
                        type_name: other.type_name(),
                    }
                    .into())
                }
            };

            stack.push(result);
        }
        Instruction::CreateList => {
            stack.push(Value::List(Vec::new()));
        }
        Instruction::AddToList => {
            let (list, item) = stack.pop2()?;
            match list {
                Value::List(mut list) => {
                    list.push(item);
                    stack.push(Value::List(list));
                }
                other => {
                    return Err(RuntimeError::IncorrectType {
                        expected: Type::List,
                        actual: other.type_name(),
                    }
                    .into());
                }
            }
        }
        Instruction::SaveGeometry => {
            match stack.pop()? {
                Value::Geometry(g) => {
                    callbacks.consume_geometry(g);
                }
                Value::Undef => {
                    // ignore it
                }
                other => {
                    return Err(RuntimeError::IncorrectType {
                        expected: Type::Geometry,
                        actual: other.type_name(),
                    }
                    .into())
                }
            }
        }
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
