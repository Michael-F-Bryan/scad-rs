use im::{OrdMap, Vector};
use rowan::SyntaxNode;

use crate::{
    hir::{Id, Text},
    syntax::OpenSCAD,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ControlFlowNode {
    pub instructions: Vector<Instruction>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constant {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Namespace {
    pub names: OrdMap<Text, Value>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Value {
    Function(Id),
    Module(Id),
    Constant(Id),
}
