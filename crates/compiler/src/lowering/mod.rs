mod bytecode;

pub use self::query::{Lowering, LoweringStorage};

mod query {
    use crate::{lowering::bytecode::compile, parsing::Parsing, Diagnostics};
    use scad_bytecode::Program;

    /// Lowering from the [`ast`] representation to a [`Program`].
    #[salsa::query_group(LoweringStorage)]
    pub trait Lowering: Parsing {
        fn compile(&self) -> (Program, Diagnostics);
    }
}
