#![warn(elided_lifetimes_in_paths)]

mod db;
mod diagnostics;
pub mod lowering;
pub mod parsing;
mod spans;
mod text;

pub use crate::{
    db::Database,
    diagnostics::{Diagnostic, Diagnostics, IntoDiagnostic, Severity},
    spans::{Location, Spanned},
    text::Text,
};
