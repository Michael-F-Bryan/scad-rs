use std::{
    ops::{Add, Div, Mul, Neg, Not, Sub},
    sync::Arc,
};

use scad_bytecode::Constant;

use crate::RuntimeError;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Undef,
    Number(f64),
    Boolean(bool),
    String(Arc<str>),
}

impl Value {
    #[inline]
    pub const fn type_name(&self) -> &'static str {
        match self {
            Value::Undef => "undef",
            Value::Number(_) => "number",
            Value::Boolean(_) => "bool",
            Value::String(_) => "string",
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, RuntimeError>;

    #[inline]
    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            other => Err(RuntimeError::InvalidUnaryOperation {
                operation: "negate",
                type_name: other.type_name(),
            }),
        }
    }
}

impl Not for Value {
    type Output = Result<Value, RuntimeError>;

    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            other => Err(RuntimeError::InvalidUnaryOperation {
                operation: "not",
                type_name: other.type_name(),
            }),
        }
    }
}

impl Add for Value {
    type Output = Result<Value, RuntimeError>;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{a}{b}").into())),
            (lhs, rhs) => Err(RuntimeError::InvalidBinaryOperation {
                operation: "add",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
            }),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, RuntimeError>;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{a}{b}").into())),
            (lhs, rhs) => Err(RuntimeError::InvalidBinaryOperation {
                operation: "subtract",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
            }),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, RuntimeError>;

    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (lhs, rhs) => Err(RuntimeError::InvalidBinaryOperation {
                operation: "multiply",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
            }),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, RuntimeError>;

    #[inline]
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
            (lhs, rhs) => Err(RuntimeError::InvalidBinaryOperation {
                operation: "divide",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
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
