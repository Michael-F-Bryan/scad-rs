//! The high-level intermediate representation.

use im::Vector;
use rowan::{ast::AstNode, TextRange};
use scad_syntax::ast;

use crate::Text;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub arguments: Vector<Argument>,
    pub return_value: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Argument {
    pub name: Text,
    pub span: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDefinition {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleDefinition {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Integer,
    Float,
    String,
    Undefined,
    /// A type error.
    Error,
    /// The type isn't yet known.
    Unknown,
}

ast_node_union! {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Item {
        Function(ast::NamedFunctionDefinition),
        Module(ast::NamedModuleDefinition),
        Constant(ast::AssignmentStatement),
    }
}

impl Item {
    pub fn name(&self) -> Option<Text> {
        match self {
            Item::Function(f) => f.ident_token().map(|id| id.text().into()),
            Item::Module(m) => m.ident_token().map(|id| id.text().into()),
            Item::Constant(a) => a.assignment()?.ident_token().map(|id| id.text().into()),
        }
    }
}

pub trait Spanned {
    fn span(&self) -> TextRange;
}

impl<A: AstNode> Spanned for A {
    fn span(&self) -> TextRange {
        self.syntax().text_range()
    }
}
