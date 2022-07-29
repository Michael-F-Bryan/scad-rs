//! The high-level intermediate representation of a program.
//!
//! In theory, this [control-flow graph][cfg]-based representation should give
//! you enough context to evaluate a [`Package`] using a tree-walking
//! interpreter.
//!
//! [cfg]: https://en.wikipedia.org/wiki/Control-flow_graph

mod id;
mod types;

pub use self::{
    id::{Id, Identifiers},
    types::*,
};
