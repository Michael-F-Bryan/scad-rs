mod generated;
mod lexer;
pub mod parser;

pub use crate::{generated::syntax_kind::SyntaxKind, lexer::tokenize};
