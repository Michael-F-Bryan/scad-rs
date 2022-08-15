#![warn(elided_lifetimes_in_paths)]

#[macro_use]
mod macros;

mod db;
mod diagnostics;
pub mod hir;
pub mod lowering;
mod parsing;
mod spans;
mod text;

pub use crate::{
    diagnostics::{Diagnostic, Diagnostics, IntoDiagnostic, Severity},
    spans::{Location, Spanned},
    text::Text,
};
