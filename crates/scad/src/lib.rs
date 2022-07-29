//! A Rust implementation of the OpenSCAD virtual machine.

pub mod ast;
mod diagnostics;
pub mod hir;
pub mod lowering;
pub mod syntax;
mod text;

pub use crate::{
    diagnostics::{Diagnostic, Diagnostics, Severity},
    text::Text,
};
