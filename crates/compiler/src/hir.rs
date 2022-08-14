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
    Boolean,
    Integer,
    Float,
    String,
    Undefined,
    /// A type error.
    Error,
}

impl Type {
    pub const fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }
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
        let ident = match self {
            Item::Function(f) => f.ident_token()?,
            Item::Module(m) => m.ident_token()?,
            Item::Constant(a) => a.assignment()?.ident_token()?,
        };

        Some(ident.text().into())
    }

    pub fn as_function(&self) -> Option<&ast::NamedFunctionDefinition> {
        match self {
            Item::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_module(&self) -> Option<&ast::NamedModuleDefinition> {
        match self {
            Item::Module(m) => Some(m),
            _ => None,
        }
    }

    pub fn as_constant(&self) -> Option<&ast::AssignmentStatement> {
        match self {
            Item::Constant(c) => Some(c),
            _ => None,
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

ast_node_union! {
    /// The place a name can first be introduced.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum NameDefinitionSite {
        Function(ast::NamedFunctionDefinition),
        Module(ast::NamedModuleDefinition),
        Constant(ast::AssignmentStatement),
        Parameter(ast::Parameter),
    }
}

impl NameDefinitionSite {
    pub fn name(&self) -> Option<Text> {
        let ident = match self {
            NameDefinitionSite::Function(f) => f.ident_token()?,
            NameDefinitionSite::Module(m) => m.ident_token()?,
            NameDefinitionSite::Constant(a) => a.assignment()?.ident_token()?,
            NameDefinitionSite::Parameter(p) => p.ident()?,
        };

        Some(ident.text().into())
    }

    /// Try to get the expression this item would evaluate to, if it is defined.
    pub fn body(&self) -> Option<ast::Expr> {
        match self {
            NameDefinitionSite::Function(f) => f.expr(),
            NameDefinitionSite::Module(_) => None,
            NameDefinitionSite::Constant(c) => c.assignment()?.expr(),
            NameDefinitionSite::Parameter(ast::Parameter::Assignment(ass)) => ass.expr(),
            NameDefinitionSite::Parameter(ast::Parameter::Ident(_)) => None,
        }
    }
}
