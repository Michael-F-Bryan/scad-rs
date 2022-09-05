pub use self::query::{Parsing, ParsingStorage};

mod query {
    use scad_syntax::{ast::Package, ParseError};

    use crate::{Diagnostic, Diagnostics, Location, Severity, Text};

    #[salsa::query_group(ParsingStorage)]
    pub trait Parsing {
        /// The program's source code.
        #[salsa::input]
        fn src(&self) -> Text;

        /// Parse some source code into an AST using the [`scad_syntax`]
        /// crate.
        fn parse(&self, src: Text) -> (Package, Diagnostics);
    }

    fn parse(_: &dyn Parsing, src: Text) -> (Package, Diagnostics) {
        let tokens = scad_syntax::tokenize(&src);
        let (pkg, errors) = scad_syntax::parse(tokens);
        let mut diags = Diagnostics::empty();

        for ParseError { location, msg } in errors {
            let diag = Diagnostic {
                severity: Severity::Error,
                message: msg.into(),
                location: Location {
                    text_range: location,
                },
            };
            diags.push(diag);
        }

        (pkg, diags)
    }
}
