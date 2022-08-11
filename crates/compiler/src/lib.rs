mod diagnostics;
pub mod hir;
pub mod lowering;
mod text;

pub use crate::{
    diagnostics::{Diagnostic, Diagnostics, IntoDiagnostic, Severity},
    text::Text,
};
