pub use self::query::{Parsing, ParsingStorage};

mod query {
    use scad_syntax::ast::Package;

    use crate::{Diagnostics, Text};

    #[salsa::query_group(ParsingStorage)]
    pub trait Parsing {
        /// The program's source code.
        #[salsa::input]
        fn src(&self) -> Text;

        /// Parse the program's source code into an AST using the [`scad_syntax`]
        /// crate.
        fn ast(&self) -> (Package, Diagnostics);
    }

    fn ast(parser: &dyn Parsing) -> (Package, Diagnostics) {
        let src = parser.src();
        let tokens = scad_syntax::tokenize(&src);
        let (pkg, errors) = scad_syntax::parse(tokens);

        if !errors.is_empty() {
            todo!("Turn errors into diagnostics");
        }

        (pkg, Diagnostics::empty())
    }
}
