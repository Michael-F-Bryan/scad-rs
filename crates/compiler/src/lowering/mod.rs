mod items;
mod type_inference;

pub use self::query::{Lowering, LoweringStorage};

mod query {
    use im::{OrdMap, Vector};
    use rowan::SyntaxNode;
    use scad_syntax::{ast, OpenSCAD};

    use crate::{
        hir,
        lowering::{
            items::{named_items_in_scope, top_level_items},
            type_inference::inferred_type,
        },
        parsing::Parsing,
        Diagnostics, Text,
    };

    /// Lowering from the [`ast`] representation to [`hir`].
    #[salsa::query_group(LoweringStorage)]
    pub trait Lowering: Parsing {
        fn top_level_items(&self) -> (OrdMap<Text, hir::Item>, Diagnostics);

        /// Get all names that are accessible from a particular node.
        fn named_items_in_scope(
            &self,
            target: SyntaxNode<OpenSCAD>,
        ) -> Vector<(Text, hir::NameDefinitionSite)>;

        /// Try to infer the type of an [`ast::Expr`], returning
        /// [`hir::Type::Error`] if inference fails.
        fn inferred_type(&self, expr: ast::Expr) -> hir::Type;
    }
}
