use std::{
    fmt::{self, Display, Formatter},
    sync::Arc,
};

#[derive(Debug, Clone, PartialEq, Eq)]
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
    UnknownVariable {
        name: Arc<str>,
    },
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
        }
    }
}

impl std::error::Error for RuntimeError {}
