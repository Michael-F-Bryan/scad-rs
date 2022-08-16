//! The high-level intermediate representation.

use im::Vector;
use rowan::TextRange;
use salsa::InternKey;
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
    /// Is this a [`Type::Error`]?
    pub const fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }

    /// Is this a numeric type (i.e. [`Type::Integer`] or [`Type::Float`])?
    pub const fn is_numeric(&self) -> bool {
        matches!(self, Type::Integer | Type::Float)
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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionId(salsa::InternId);

impl InternKey for DefinitionId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        DefinitionId(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
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

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct BasicBlock {
    pub statements: Vector<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Assignment {
        variable_name: Variable,
        value: AssignmentValue,
    },
    BinaryOp {
        dest: Variable,
        lhs: AssignmentValue,
        op: BinOp,
        rhs: AssignmentValue,
    },
    ModuleInvocation {
        dest: Variable,
        module: DefinitionId,
        inputs: Vector<Input>,
        child: Option<Variable>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Input {
    Anonymous(AssignmentValue),
    Named { name: Text, value: AssignmentValue },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    GreaterThanEquals,
    GreaterThan,
    DoubleEquals,
    LessThanEquals,
    LessThan,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variable {
    Named(DefinitionId),
    Anonymous(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignmentValue {
    Literal(Literal),
    Variable(Variable),
}

impl From<Literal> for AssignmentValue {
    fn from(v: Literal) -> Self {
        Self::Literal(v)
    }
}

impl From<Variable> for AssignmentValue {
    fn from(v: Variable) -> Self {
        Self::Variable(v)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    Float(noisy_float::types::R64),
    Boolean(bool),
    String(Text),
    Undefined,
}
