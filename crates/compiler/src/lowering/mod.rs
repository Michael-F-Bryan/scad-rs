mod bytecode;

pub use self::query::{Lowering, LoweringStorage};

mod query {
    use crate::{
        lowering::bytecode::{compile, compile_package},
        parsing::Parsing,
        Diagnostics,
    };
    use scad_bytecode::{Chunk, Program};
    use scad_syntax::ast;

    /// Lowering from the [`ast`] representation to a [`Program`].
    #[salsa::query_group(LoweringStorage)]
    pub trait Lowering: Parsing {
        fn compile(&self) -> (Program, Diagnostics);

        fn compile_package(&self, ast: ast::Package) -> (Chunk, Diagnostics);
    }
}
