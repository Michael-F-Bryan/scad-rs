use crate::{RuntimeError, Value};

/// Contextual information provided to builtin functions.
pub trait Context {
    fn print(&mut self, values: &[Value]) -> Result<(), RuntimeError>;
}
