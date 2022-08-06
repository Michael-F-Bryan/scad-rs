mod expressions;
mod token_set;
mod top_level;

pub use self::token_set::TokenSet;
pub(crate) use self::top_level::*;

use crate::{ast::Package, parser::Parser, SyntaxKind};
use rowan::{ast::AstNode, TextRange};

pub fn parse<'a>(
    tokens: impl IntoIterator<Item = (SyntaxKind, &'a str)>,
) -> (Package, Vec<ParseError>) {
    let mut parser = Parser::new(tokens);
    package(&mut parser);
    let (node, errors) = parser.finish();

    (Package::cast(node).unwrap(), errors)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub location: TextRange,
    pub msg: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_the_kitchen_sink() {
        let src = r#"
            include "foo/bar.scad"
            use "baz.scad"
            a = 42;
            b = (42);
            c = 1 + 1;
        "#;

        let (pkg, errors) = parse(crate::tokenize(src));

        eprintln!("{pkg:#?}");

        let statements: Vec<_> = pkg.statements().collect();
        assert_eq!(statements.len(), 5);

        let _include = statements[0].as_include().unwrap();
        let _use = statements[1].as_use().unwrap();
        let _a = statements[2].as_assignment().unwrap();
        let _b = statements[3].as_assignment().unwrap();

        let c = statements[4].as_assignment().unwrap();
        let _bin_expr = c.value().unwrap().as_bin_expr().unwrap();

        insta::assert_debug_snapshot!(pkg);
        insta::assert_debug_snapshot!(errors);
    }
}
