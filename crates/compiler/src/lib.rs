#[macro_use]
mod macros;

mod diagnostics;
pub mod hir;
pub mod lowering;
mod parsing;
mod text;
mod db;

pub use crate::{
    diagnostics::{Diagnostic, Diagnostics, IntoDiagnostic, Severity},
    text::Text,
};
