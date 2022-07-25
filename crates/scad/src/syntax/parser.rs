use std::{
    collections::{HashSet, VecDeque},
    ops::{Deref, DerefMut},
};

use crate::syntax::{ast::*, lexer::OpenSCAD, SyntaxKind, SyntaxKind::*};
use rowan::{ast::AstNode, GreenNodeBuilder, SyntaxNode, TextRange};

pub fn parse(text: &str) -> (Package, Vec<ParseError>) {
    let mut parser = Parser::new(text);
    parser.parse_package();
    parser.finish()
}

#[derive(Debug)]
struct Parser<'src> {
    builder: GreenNodeBuilder<'static>,
    tokens: VecDeque<(SyntaxKind, &'src str)>,
    errors: Vec<ParseError>,
    current_index: usize,
}

impl<'src> Parser<'src> {
    fn new(src: &'src str) -> Self {
        Parser {
            builder: GreenNodeBuilder::new(),
            tokens: crate::syntax::tokenize(src).collect(),
            errors: Vec::new(),
            current_index: 0,
        }
    }

    fn finish<A>(self) -> (A, Vec<ParseError>)
    where
        A: AstNode<Language = OpenSCAD>,
    {
        let Parser {
            builder, errors, ..
        } = self;

        let root_node = SyntaxNode::<OpenSCAD>::new_root(builder.finish());
        let kind = root_node.kind();

        match A::cast(root_node) {
            Some(root) => (root, errors),
            None => {
                let name = std::any::type_name::<A>();
                panic!("Unable to convert a {kind:?} into a {name}");
            }
        }
    }

    fn parse_package(&mut self) {
        todo!();
    }

    fn parse_ident(&mut self) -> Result<(), ParseError> {
        let mut parser = self.start_node(SyntaxKind::IDENTIFIER);
        parser.expect(SyntaxKind::IDENTIFIER)?;

        Ok(())
    }

    fn parse_expr(&mut self) -> Result<(), ParseError> {
        let mut parser = self.start_node(SyntaxKind::EXPR);
        parser.skip_whitespace();

        let kind = parser.current().ok_or(ParseError::eof(EXPR))?;

        let expected = [NUMBER, TRUE, FALSE, UNDEF, STRING];

        if expected.contains(&kind) {
            parser.consume_atom();
        } else {
            return Err(ParseError::BadSyntax {
                found: kind,
                location: parser.current_location().unwrap(),
                expected: expected.into_iter().collect(),
            });
        }

        Ok(())
    }

    fn current_location(&self) -> Option<TextRange> {
        let (_, text) = self.peek()?;

        let start = self.current_index;
        let end = start + text.len();
        let location = TextRange::new(start.try_into().unwrap(), end.try_into().unwrap());

        Some(location)
    }

    fn consume_atom(&mut self) {
        let kind = self.current().unwrap();
        let mut parser = self.start_node(kind);
        parser.bump();
    }

    fn parse_assignment(&mut self) -> Result<(), ParseError> {
        let mut parser = self.start_node(SyntaxKind::ASSIGNMENT);
        parser.skip_whitespace();
        parser.parse_ident()?;
        parser.expect(SyntaxKind::EQUALS)?;
        parser.parse_expr()?;

        Ok(())
    }

    fn expect(&mut self, kind: SyntaxKind) -> Result<(), ParseError> {
        self.skip_whitespace();

        let (current, text) = self.peek().ok_or(ParseError::eof(kind))?;

        if current == kind {
            self.bump();
            Ok(())
        } else {
            let start = self.current_index;
            let end = start + text.len();
            let location = TextRange::new(start.try_into().unwrap(), end.try_into().unwrap());
            Err(ParseError::BadSyntax {
                found: current,
                location,
                expected: [kind].into_iter().collect(),
            })
        }
    }

    fn bump(&mut self) {
        if let Some((kind, text)) = self.tokens.pop_front() {
            self.current_index += text.len();
            self.builder.token(kind.into(), text);
        }
    }

    fn current(&self) -> Option<SyntaxKind> {
        self.peek().map(|(kind, _)| kind)
    }

    fn peek(&self) -> Option<(SyntaxKind, &'src str)> {
        self.tokens.front().copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(kind) = self.current() {
            match kind {
                WHITESPACE | COMMENT => self.bump(),
                _ => break,
            }
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) -> impl DerefMut<Target = Parser<'src>> + '_ {
        struct Guard<'p, 'src> {
            parser: &'p mut Parser<'src>,
        }
        impl<'p, 'src> Deref for Guard<'p, 'src> {
            type Target = Parser<'src>;

            fn deref(&self) -> &Self::Target {
                &*self.parser
            }
        }
        impl<'p, 'src> DerefMut for Guard<'p, 'src> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut *self.parser
            }
        }
        impl<'p, 'src> Drop for Guard<'p, 'src> {
            fn drop(&mut self) {
                self.parser.builder.finish_node();
            }
        }

        self.builder.start_node(kind.into());

        Guard { parser: self }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEof {
        expected: SyntaxKind,
    },
    BadSyntax {
        found: SyntaxKind,
        location: TextRange,
        expected: HashSet<SyntaxKind>,
    },
}

impl ParseError {
    fn eof(expected: SyntaxKind) -> Self {
        ParseError::UnexpectedEof { expected }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(test)]
    mod tests {
        use super::*;

        macro_rules! parse_test {
            ($name:ident, $src:expr, $parse:expr, $assertions:expr) => {
                #[test]
                fn $name() {
                    let mut parser = Parser::new($src);

                    let p: fn(&mut Parser) = $parse;
                    p(&mut parser);

                    let (value, errors) = parser.finish();

                    assert_eq!(errors, Vec::new());
                    $assertions(value);
                }
            };
        }

        parse_test!(
            parse_assignment,
            "x = 42",
            |p| p.parse_assignment().unwrap(),
            |a: Assignment| {
                assert_eq!(a.identifier().unwrap().name(), "x");
                assert_eq!(a.expression().unwrap().number().unwrap().literal(), "42");
            }
        );

        parse_test!(
            parse_true,
            "true",
            |p| p.parse_expr().unwrap(),
            |expr: Expr| {
                let b = expr.boolean().unwrap();
                assert_eq!(b.value(), true);
            }
        );

        parse_test!(
            parse_false,
            "false",
            |p| p.parse_expr().unwrap(),
            |expr: Expr| {
                let b = expr.boolean().unwrap();
                assert_eq!(b.value(), false);
            }
        );

        parse_test!(
            parse_string_literal,
            r#""Hello, World!""#,
            |p| p.parse_expr().unwrap(),
            |expr: Expr| {
                let s = expr.string().unwrap();
                assert_eq!(s.value(), "Hello, World!");
            }
        );

        parse_test!(
            parse_undef,
            "undef",
            |p| p.parse_expr().unwrap(),
            |expr: Expr| {
                let _ = expr.undef().unwrap();
            }
        );
    }
}
