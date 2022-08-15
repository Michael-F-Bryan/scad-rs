mod basic_blocks;
mod items;
mod type_inference;

pub use self::query::{Lowering, LoweringStorage};

mod query {
    use im::{OrdMap, Vector};
    use scad_syntax::{ast, SyntaxNode};

    use crate::{
        hir,
        lowering::{
            basic_blocks::basic_blocks,
            items::{ident_declaration, named_items_in_scope, top_level_items},
            type_inference::inferred_type,
        },
        parsing::Parsing,
        Diagnostics, Text,
    };

    /// Lowering from the [`ast`] representation to [`hir`].
    #[salsa::query_group(LoweringStorage)]
    pub trait Lowering: Parsing {
        fn top_level_items(&self) -> (OrdMap<Text, hir::Item>, Diagnostics);

        #[salsa::interned]
        fn declaration(&self, site: hir::NameDefinitionSite) -> hir::DefinitionId;

        /// Get all names that are accessible from a particular node.
        fn named_items_in_scope(&self, target: SyntaxNode) -> Vector<(Text, hir::DefinitionId)>;

        /// Try to infer the type of an [`ast::Expr`], returning
        /// [`hir::Type::Error`] if inference fails.
        fn inferred_type(&self, expr: ast::Expr) -> hir::Type;

        /// Find the definition for a particular identifier.
        ///
        /// # Panics
        ///
        /// The provided [`scad_syntax::SyntaxToken`] must be a
        /// [`scad_syntax::SyntaxKind::IDENT`] attached to a
        /// [`SyntaxNode`].
        fn ident_declaration(&self, name: Text, scope: SyntaxNode) -> Option<hir::DefinitionId>;

        fn basic_blocks(&self, statements: Vector<ast::Statement>) -> Vector<hir::BasicBlock>;
    }
}
