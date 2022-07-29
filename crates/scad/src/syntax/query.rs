use im::Vector;

use crate::{ast::Package, syntax::ParseError, Text};

#[salsa::query_group(ParseStorage)]
pub trait Parse {
    #[salsa::input]
    fn source_code(&self) -> Text;

    fn ast(&self) -> (Package, Vector<ParseError>);
}

fn ast(parser: &dyn Parse) -> (Package, Vector<ParseError>) {
    let source_code = parser.source_code();
    crate::syntax::parse(&source_code)
}
