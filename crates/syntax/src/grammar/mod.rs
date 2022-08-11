#[macro_use]
mod macros;

mod expressions;
mod token_set;
mod top_level;

pub use self::token_set::TokenSet;
pub(crate) use self::top_level::*;

use crate::{ast::Package, parser::Parser, SyntaxKind};
use rowan::{ast::AstNode, TextRange};

/// Parse a set of tokens into a [`Package`].
pub fn parse<'a>(
    tokens: impl IntoIterator<Item = (SyntaxKind, &'a str)>,
) -> (Package, Vec<ParseError>) {
    let mut parser = Parser::new(tokens);
    package(&mut parser);
    let (node, errors) = parser.finish();

    (Package::cast(node).unwrap(), errors)
}

/// A basic parser error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// Where the error occurred in the source code.
    pub location: TextRange,
    /// The error message.
    pub msg: String,
}
