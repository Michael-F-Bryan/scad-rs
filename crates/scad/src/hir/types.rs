use std::str::FromStr;

use im::{OrdMap, Vector};
use noisy_float::types::R64;
use rowan::SyntaxNode;

use crate::{hir::Id, syntax::OpenSCAD, Text};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Package {
    pub functions: OrdMap<Id, Function>,
    pub modules: OrdMap<Id, Module>,
    pub constants: OrdMap<Id, Constant>,
    pub global_namespace: Namespace,
    pub syntax: OrdMap<Id, SyntaxNode<OpenSCAD>>,
    pub script: ControlFlowGraph,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ControlFlowGraph {
    /// The index of the first node in this [`ControlFlowGraph`].
    pub entry_point: usize,
    pub nodes: Vector<ControlFlowNode>,
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        let nodes = Vector::from_iter([ControlFlowNode {
            instructions: Vector::new(),
            exit: Continuation::Return,
        }]);
        Self {
            entry_point: 0,
            nodes,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ControlFlowNode {
    pub instructions: Vector<Instruction>,
    pub exit: Continuation,
}

/// What to do once you get to the end of a [`ControlFlowNode`]'s instructions.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Continuation {
    Return,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constant {
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    IntegerLiteral(u64),
    FloatLiteral(FloatLiteral),
    StringLiteral(Text),
}

impl From<u64> for Expr {
    fn from(u: u64) -> Self {
        Expr::IntegerLiteral(u)
    }
}

impl From<FloatLiteral> for Expr {
    fn from(f: FloatLiteral) -> Self {
        Expr::FloatLiteral(f)
    }
}

impl From<Text> for Expr {
    fn from(s: Text) -> Self {
        Expr::StringLiteral(s)
    }
}

impl PartialEq<FloatLiteral> for Expr {
    fn eq(&self, other: &FloatLiteral) -> bool {
        match self {
            Expr::FloatLiteral(f) => f == other,
            _ => false,
        }
    }
}

impl PartialEq<f64> for Expr {
    fn eq(&self, other: &f64) -> bool {
        match self {
            Expr::FloatLiteral(f) => f == other,
            _ => false,
        }
    }
}

impl PartialEq<u64> for Expr {
    fn eq(&self, other: &u64) -> bool {
        match self {
            Expr::IntegerLiteral(i) => i == other,
            _ => false,
        }
    }
}

impl PartialEq<&str> for Expr {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Expr::StringLiteral(s) => s == other,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum FloatLiteral {
    Real(R64),
    NaN,
}

impl FloatLiteral {
    fn value(self) -> f64 {
        match self {
            FloatLiteral::Real(f) => f.raw(),
            FloatLiteral::NaN => f64::NAN,
        }
    }
}

impl FromStr for FloatLiteral {
    type Err = <f64 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let float = s.parse()?;

        match R64::try_new(float) {
            Some(f) => Ok(FloatLiteral::Real(f)),
            None => Ok(FloatLiteral::NaN),
        }
    }
}

impl PartialEq<f64> for FloatLiteral {
    fn eq(&self, other: &f64) -> bool {
        self.value() == *other
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Namespace {
    pub names: OrdMap<Text, Value>,
}

impl Namespace {
    pub fn insert(&mut self, name: impl Into<Text>, value: Value) {
        self.names.insert(name.into(), value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.names.get(name).cloned()
    }

    pub fn is_empty(&self) -> bool {
        self.names.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Value {
    Function(Id),
    Module(Id),
    Constant(Id),
}

impl Value {
    pub fn id(self) -> Id {
        match self {
            Value::Function(id) | Value::Module(id) | Value::Constant(id) => id,
        }
    }
}

impl From<Value> for Id {
    fn from(v: Value) -> Self {
        v.id()
    }
}
