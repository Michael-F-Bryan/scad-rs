mod diagnostics;
pub mod lowering;
pub mod hir;
mod text;

pub use crate::{
    diagnostics::{Diagnostic, Diagnostics, IntoDiagnostic, Severity},
    text::Text,
};
