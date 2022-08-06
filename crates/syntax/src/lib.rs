// generated code
#[allow(clippy::redundant_clone, unreachable_patterns)]
pub mod ast;
#[macro_use]
mod syntax_kind;

mod grammar;
mod lexer;
pub mod parser;

pub use crate::{grammar::parse, lexer::tokenize, syntax_kind::SyntaxKind};

/// A tag type used to represent the OpenSCAD language.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct OpenSCAD;

impl rowan::Language for OpenSCAD {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from_code(raw.0).unwrap()
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}
