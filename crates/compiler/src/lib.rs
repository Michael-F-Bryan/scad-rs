#![warn(elided_lifetimes_in_paths)]

mod db;
mod diagnostics;
pub mod lowering;
mod parsing;
mod spans;
mod text;

pub use crate::{
    diagnostics::{Diagnostic, Diagnostics, IntoDiagnostic, Severity},
    spans::{Location, Spanned},
    text::Text,
};
