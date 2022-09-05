use std::{
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeError {
    StackUnderflow,
    InvalidUnaryOperation {
        operation: &'static str,
        type_name: Type,
    },
    InvalidBinaryOperation {
        operation: &'static str,
        lhs: Type,
        rhs: Type,
    },
    UnknownVariable {
        name: Arc<str>,
    },
    IncorrectType {
        expected: Type,
        actual: Type,
    },
    NotCallable {
        type_name: Type,
    },
    ConversionError(ConversionError),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
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
            RuntimeError::UnknownVariable { name } => {
                write!(f, "\"{name}\" is not defined in this scope")
            }
            RuntimeError::IncorrectType { expected, actual } => {
                write!(f, "Expected a {expected}, but found a {actual}")
            }
            RuntimeError::NotCallable { type_name } => {
                write!(f, "A {type_name} is not callable")
            }
            RuntimeError::ConversionError(_) => write!(f, "Conversion failed"),
        }
    }
}

impl std::error::Error for RuntimeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            RuntimeError::ConversionError(c) => Some(c),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ConversionError {
    pub from: Type,
    pub to: Type,
}

impl Display for ConversionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ConversionError { from, to } = self;
        write!(f, "Unable to convert {from} to a {to}")
    }
}

impl std::error::Error for ConversionError {}
