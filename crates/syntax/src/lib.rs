//! A parser and lexer for the OpenSCAD language.
//!
//! There are 4 key components in this crate,
//!
//! - [`SyntaxKind`]: an enum representing the various terminal and
//!   non-terminal items in the OpenSCAD grammar
//! - the [`ast`] module: strongly-typed wrappers around the loosely-typed
//!   [`rowan::SyntaxNode`]
//! - [`tokenize()`] - Convert a string into a stream of tokens (a
//!   [`SyntaxKind`] and its text)
//! - [`parse()`] - Parse a stream of tokens into the top-level [`ast::Package`]
//!   it contains
//!
//! Here is a typical use of this crate:
//!
//! ```rust
//! let src = r#"
//!   use <MCAD/math.scad>
//!
//!   x = 1;
//!   y = 2;
//!   x_plus_y = x + y;
//! "#;
//!
//! let tokens = scad_syntax::tokenize(src);
//! let (package, errors) = scad_syntax::parse(tokens);
//!
//! assert!(errors.is_empty(), "{errors:?}");
//! ```
//!
//! An important thing to notice is that we will *always* get a [`ast::Package`]
//! back from parsing, even if the tokens contain utter garbage. The parser
//! borrows a lot of inspiration from `rust-analyzer`, so a lot of things
//! mentioned in [their architecture docs][arch] will also apply here.
//!
//! In particular, we have the following design goals:
//!
//! - Syntax trees are lossless, or full fidelity. All comments and whitespace
//!   get preserved.
//! - Syntax trees are semantic-less. They describe strictly the structure of a
//!   sequence of characters, they don't have hygiene, name resolution or type
//!   information attached.
//! - Syntax trees are simple value types. It is possible to create trees for a
//!   syntax without any external context.
//! - Syntax trees have intuitive traversal API (parent, children, siblings,
//!   etc).
//! - Parsing is lossless (even if the input is invalid, the tree produced by
//!   the parser represents it exactly).
//! - Parsing is resilient (even if the input is invalid, parser tries to see as
//!   much syntax tree fragments in the input as it can).
//!
//! [arch]: https://github.com/rust-lang/rust-analyzer

#![warn(elided_lifetimes_in_paths)]

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
// generated code
#[allow(clippy::redundant_clone, unreachable_patterns)]
pub mod ast;
#[macro_use]
mod syntax_kind;

mod grammar;
mod lexer;
mod parser;
mod methods;

pub use crate::{
    grammar::{parse, ParseError},
    lexer::tokenize,
    syntax_kind::SyntaxKind,
};

/// A tag type used by [`rowan`] to represent the OpenSCAD language.
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
