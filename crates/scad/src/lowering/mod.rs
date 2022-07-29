//! Lowering a program from its loosely-typed [`crate::ast`] representation to a
//! strongly-typed [`crate::hir`].

mod query;

pub use self::query::Lowering;
