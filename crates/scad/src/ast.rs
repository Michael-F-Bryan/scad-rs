//! The *Abstract Syntax Tree* representation of a program.
//!
//! Every program lives in a [`Package`].

use std::str::FromStr;

use rowan::{ast::AstNode, SyntaxNode, SyntaxText, TextSize};

use crate::syntax::{OpenSCAD, SyntaxKind};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Package(SyntaxNode<OpenSCAD>);

impl Package {
    pub fn statements(&self) -> impl Iterator<Item = Statement> {
        self.0.children().filter_map(Statement::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Statement(SyntaxNode<OpenSCAD>);

impl Statement {
    pub fn as_assignment(&self) -> Option<Assignment> {
        self.0.children().find_map(Assignment::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Assignment(SyntaxNode<OpenSCAD>);

impl Assignment {
    pub fn identifier(&self) -> Option<Ident> {
        self.0.children().find_map(|c| Ident::cast(c))
    }

    pub fn expression(&self) -> Option<Expr> {
        self.0.children().find_map(|c| Expr::cast(c))
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Ident(SyntaxNode<OpenSCAD>);

impl Ident {
    pub fn name(&self) -> SyntaxText {
        self.0.text()
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Expr(SyntaxNode<OpenSCAD>);

impl Expr {
    pub fn number(&self) -> Option<Number> {
        self.0.children().find_map(Number::cast)
    }

    pub fn boolean(&self) -> Option<Boolean> {
        self.0.children().find_map(Boolean::cast)
    }

    pub fn string(&self) -> Option<StringLiteral> {
        self.0.children().find_map(StringLiteral::cast)
    }

    pub fn undef(&self) -> Option<Undefined> {
        self.0.children().find_map(Undefined::cast)
    }

    pub fn binary_op(&self) -> Option<(Expr, SyntaxNode<OpenSCAD>, Expr)> {
        let useful_nodes: Vec<_> = self
            .0
            .children()
            .filter(|c| match c.kind() {
                SyntaxKind::COMMENT | SyntaxKind::WHITESPACE => false,
                _ => true,
            })
            .collect();

        let [left, op, right]: [SyntaxNode<OpenSCAD>; 3] = dbg!(useful_nodes.try_into().ok()?);

        let left = Expr::cast(left)?;
        let right = Expr::cast(right)?;

        Some((left, op, right))
    }

    pub fn lookup(&self) -> Option<Lookup> {
        self.0.children().find_map(Lookup::cast)
    }

    pub fn function_call(&self) -> Option<FunctionCall> {
        self.0.children().find_map(FunctionCall::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Lookup(SyntaxNode<OpenSCAD>);

impl Lookup {
    pub fn ident(&self) -> Option<Ident> {
        self.0.children().find_map(Ident::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FunctionCall(SyntaxNode<OpenSCAD>);

impl FunctionCall {
    pub fn ident(&self) -> Option<Ident> {
        self.0.children().find_map(Ident::cast)
    }

    pub fn parameters(&self) -> Option<Parameters> {
        self.0.children().find_map(Parameters::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Parameters(SyntaxNode<OpenSCAD>);

impl Parameters {
    pub fn iter(&self) -> impl Iterator<Item = Parameter> {
        self.0.children().filter_map(Parameter::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Parameter(SyntaxNode<OpenSCAD>);

impl Parameter {
    pub fn named(&self) -> Option<Assignment> {
        self.0.children().find_map(Assignment::cast)
    }

    pub fn positional(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Number(SyntaxNode<OpenSCAD>);

impl Number {
    pub fn literal(&self) -> SyntaxText {
        self.0.text()
    }

    pub fn parse<T: FromStr>(&self) -> Result<T, T::Err> {
        let repr = self.literal().to_string();
        repr.parse()
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Boolean(SyntaxNode<OpenSCAD>);

impl Boolean {
    pub fn literal(&self) -> SyntaxText {
        self.0.text()
    }

    pub fn value(&self) -> bool {
        match self.0.kind() {
            SyntaxKind::TRUE_KW => true,
            SyntaxKind::FALSE_KW => false,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct StringLiteral(SyntaxNode<OpenSCAD>);

impl StringLiteral {
    pub fn literal(&self) -> SyntaxText {
        self.0.text()
    }

    pub fn value(&self) -> String {
        let full_range = self.0.text_range();

        self.literal()
            .slice(TextSize::from(1)..full_range.len() - TextSize::from(1))
            .to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Undefined(SyntaxNode<OpenSCAD>);

macro_rules! impl_ast_node {
    ($(
        $name:ident => $kind:pat
    ),* $(,)?) => {
        $(

            impl AstNode for $name {
                type Language = OpenSCAD;

                fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
                where
                    Self: Sized,
                {
                    match kind {
                        $kind => true,
                        _ => false,
                    }
                }

                fn cast(node: SyntaxNode<Self::Language>) -> Option<Self>
                where
                    Self: Sized,
                {
                    if $name::can_cast(node.kind()) {
                        Some($name(node))
                    } else {
                        None
                    }
                }

                fn syntax(&self) -> &SyntaxNode<Self::Language> {
                    &self.0
                }
            }
        )*
    };
}

impl_ast_node! {
    Package => SyntaxKind::PACKAGE,
    Ident => SyntaxKind::IDENTIFIER,
    Expr => SyntaxKind::EXPR,
    Assignment => SyntaxKind::ASSIGNMENT,
    Number => SyntaxKind::NUMBER_LIT,
    Boolean => SyntaxKind::TRUE_KW | SyntaxKind::FALSE_KW,
    StringLiteral => SyntaxKind::STRING_LIT,
    Undefined => SyntaxKind::UNDEF_KW,
    Statement => SyntaxKind::STATEMENT,
    Lookup => SyntaxKind::LOOKUP,
    FunctionCall => SyntaxKind::FUNCTION_CALL,
    Parameters => SyntaxKind::PARAMETERS,
    Parameter => SyntaxKind::PARAMETER,
}
