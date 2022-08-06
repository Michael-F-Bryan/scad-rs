pub mod ast;
mod lexer;
pub mod parser;
mod syntax_kind;

pub use crate::{lexer::tokenize, syntax_kind::SyntaxKind};

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
