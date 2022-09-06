use std::{
    convert::TryFrom,
    fmt::{self, Debug, Display, Formatter},
    ops::{Add, Div, Mul, Neg, Not, Sub},
    sync::Arc,
};

use scad_bytecode::Constant;

use crate::{Context, ConversionError, Geometry, RuntimeError};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Undef,
    Number,
    Boolean,
    String,
    List,
    BuiltinFunction,
    Geometry,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Undef => write!(f, "undef"),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::String => write!(f, "string"),
            Type::List => write!(f, "list"),
            Type::BuiltinFunction => write!(f, "builtin"),
            Type::Geometry => write!(f, "geometry"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Undef,
    Number(f64),
    Boolean(bool),
    String(Arc<str>),
    List(Vec<Value>),
    BuiltinFunction(BuiltinFunction),
    Geometry(Geometry),
}

impl Value {
    #[inline]
    pub const fn type_name(&self) -> Type {
        match self {
            Value::Undef => Type::Undef,
            Value::Number(_) => Type::Number,
            Value::Boolean(_) => Type::Boolean,
            Value::String(_) => Type::String,
            Value::List(_) => Type::List,
            Value::BuiltinFunction(_) => Type::BuiltinFunction,
            Value::Geometry(_) => Type::Geometry,
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
            Constant::Number(n) => n.raw().into(),
            Constant::String(s) => Arc::clone(s).into(),
        }
    }
}

impl From<BuiltinFunction> for Value {
    fn from(b: BuiltinFunction) -> Self {
        Value::BuiltinFunction(b)
    }
}

impl From<Arc<str>> for Value {
    fn from(s: Arc<str>) -> Self {
        Value::String(s)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Number(f)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl TryFrom<Value> for bool {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<bool, Self::Error> {
        match value {
            Value::Boolean(b) => Ok(b),
            other => Err(ConversionError {
                from: other.type_name(),
                to: Type::Boolean,
            }),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<f64, Self::Error> {
        match value {
            Value::Number(f) => Ok(f),
            other => Err(ConversionError {
                from: other.type_name(),
                to: Type::Number,
            }),
        }
    }
}

impl TryFrom<Value> for Geometry {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Geometry, Self::Error> {
        match value {
            Value::Geometry(g) => Ok(g),
            other => Err(ConversionError {
                from: other.type_name(),
                to: Type::Geometry,
            }),
        }
    }
}

#[derive(Clone)]
pub struct BuiltinFunction {
    func: Arc<dyn Fn(&mut dyn Context, Vec<Value>) -> Result<Value, RuntimeError> + Send + Sync>,
}

impl BuiltinFunction {
    pub fn new(
        func: impl Fn(&mut dyn Context, Vec<Value>) -> Result<Value, RuntimeError>
            + Send
            + Sync
            + 'static,
    ) -> Self {
        BuiltinFunction {
            func: Arc::new(func),
        }
    }

    pub fn call(&self, ctx: &mut dyn Context, args: Vec<Value>) -> Result<Value, RuntimeError> {
        (self.func)(ctx, args)
    }
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &Self) -> bool {
        // Note: There isn't a well-defined way to check whether two functions
        // are the "equal", so we just do an identity check using the function
        // object.
        let lhs = &*self.func
            as *const dyn Fn(&mut dyn Context, Vec<Value>) -> Result<Value, RuntimeError>
            as *const u8;
        let rhs = &*other.func
            as *const dyn Fn(&mut dyn Context, Vec<Value>) -> Result<Value, RuntimeError>
            as *const u8;

        std::ptr::eq(lhs, rhs)
    }
}

impl Debug for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("BuiltinFunction").finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builting_function_partialeq_only_works_on_identity() {
        fn some_func(_: &mut dyn Context, _: Vec<Value>) -> Result<Value, RuntimeError> {
            unreachable!();
        }

        let a = BuiltinFunction::new(some_func);
        let b = a.clone();

        // Both a and b point to the same Arc<dyn Fn(..)> so they are the same
        assert_eq!(a, b);

        // but a separate builtin function will always be different, even if
        // it points to the same function under the hood.
        let c = BuiltinFunction::new(some_func);
        assert_ne!(a, c);
    }
}
