use crate::{RuntimeError, Value};

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

    /// Pop the top two values from the stack.
    ///
    /// The semantics are as follows:
    ///
    /// ```rust,no_run
    /// # let mut stack = scad_runtime::Stack::default();
    /// let rhs = stack.pop()?;
    /// let lhs = stack.pop()?;
    ///
    /// assert!((lhs, rhs) == stack.pop2()?);
    /// # Ok::<(), scad_runtime::RuntimeError>(())
    /// ```
    pub fn pop2(&mut self) -> Result<(Value, Value), RuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        Ok((lhs, rhs))
    }

    /// Push a value onto the stack.
    pub fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    /// All the values on the stack, ordered from oldest to most recent.
    pub fn values(&self) -> &[Value] {
        &self.0
    }

    pub fn pop_many(&mut self, num_values: usize) -> Result<Vec<Value>, RuntimeError> {
        let len = self.0.len();
        if len < num_values {
            todo!("Handle popping too many values");
        }
        Ok(self.0.drain(len - num_values..).collect())
    }
}
