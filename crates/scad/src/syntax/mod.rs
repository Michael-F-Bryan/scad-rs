//! A lexer and parser for the OpenSCAD programming language.
//!
//! See the [grammar file][grammar] for more.
//!
//! [grammar]: https://files.openscad.org/grammar.xhtml

pub mod ast;
mod lexer;
mod parser;

pub use self::{
    lexer::{tokenize, SyntaxKind},
    parser::parse,
};
