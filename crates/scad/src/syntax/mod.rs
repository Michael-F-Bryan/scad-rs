//! The lexer and parser for the OpenSCAD programming language.
//!
//! The language grammar is derived from [this railroad diagram][grammar], with
//! tweaks to allow `LL(k)` parsing.
//!
//! [grammar]: https://files.openscad.org/grammar.xhtml

mod lexer;
mod parser;
mod query;

pub use self::{
    lexer::{tokenize, OpenSCAD, SyntaxKind},
    parser::{parse, ParseError},
    query::{Parse, ParseStorage},
};
