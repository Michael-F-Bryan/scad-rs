use std::{
    collections::VecDeque,
    ops::{Deref, DerefMut},
};

use rowan::{ast::AstNode, Checkpoint, GreenNodeBuilder, SyntaxNode};

use crate::syntax::{ast::*, lexer::OpenSCAD, SyntaxKind, SyntaxKind::*};

pub fn parse(text: &str) -> Package {
    let mut parser = Parser::new(text);
    parse_package(&mut parser);
    parser.finish()
}

#[derive(Debug)]
struct Parser<'src> {
    builder: GreenNodeBuilder<'static>,
    tokens: VecDeque<(SyntaxKind, &'src str)>,
}

impl<'src> Parser<'src> {
    fn new(src: &'src str) -> Self {
        Parser {
            builder: GreenNodeBuilder::new(),
            tokens: crate::syntax::tokenize(src).collect(),
        }
    }

    fn finish<A>(self) -> A
    where
        A: AstNode<Language = OpenSCAD>,
    {
        let Parser { builder, .. } = self;

        let root_node = SyntaxNode::<OpenSCAD>::new_root(builder.finish());
        let kind = root_node.kind();

        A::cast(root_node).unwrap_or_else(|| {
            let name = std::any::type_name::<A>();
            panic!("Unable to convert a {kind:?} into a {name}");
        })
    }

    fn expect(&mut self, kind: SyntaxKind) -> Result<(), ParseError> {
        skip_whitespace(self);

        let (k, text) = self.peek();
        if k != kind {
            self.builder.start_node(SyntaxKind::ERROR.into());
            self.token(k.into(), text);
            self.builder.finish_node();
            return Err(ParseError);
        }

        self.bump();
        Ok(())
    }

    fn token(&mut self, kind: SyntaxKind, text: &str) {
        self.builder.token(kind.into(), text);
    }

    fn bump(&mut self) {
        if let Some((kind, text)) = self.tokens.pop_front() {
            self.builder.token(kind.into(), text);
        }
    }

    fn current(&self) -> SyntaxKind {
        let (kind, _) = self.peek();
        kind
    }

    fn peek(&self) -> (SyntaxKind, &'src str) {
        self.tokens
            .front()
            .copied()
            .unwrap_or((SyntaxKind::EOF, ""))
    }

    fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
    }

    fn start_node(&mut self, kind: SyntaxKind) -> impl DerefMut<Target = Parser<'src>> + '_ {
        self.builder.start_node(kind.into());

        Guard { parser: self }
    }

    fn start_node_at(
        &mut self,
        cp: Checkpoint,
        kind: SyntaxKind,
    ) -> impl DerefMut<Target = Parser<'src>> + '_ {
        self.builder.start_node_at(cp, kind.into());

        Guard { parser: self }
    }
}

#[derive(Debug)]
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

fn parse_assignment(parser: &mut Parser<'_>) -> Result<(), ParseError> {
    let mut parser = parser.start_node(SyntaxKind::ASSIGNMENT);
    skip_whitespace(&mut parser);
    parse_ident(&mut parser)?;
    parser.expect(SyntaxKind::EQUALS)?;
    parse_expr(&mut parser)?;

    Ok(())
}

fn parse_ident(parser: &mut Parser<'_>) -> Result<(), ParseError> {
    let mut parser = parser.start_node(SyntaxKind::IDENTIFIER);
    parser.expect(SyntaxKind::IDENTIFIER)?;

    Ok(())
}

fn skip_whitespace(parser: &mut Parser<'_>) {
    loop {
        match parser.current() {
            WHITESPACE | COMMENT => parser.bump(),
            _ => break,
        }
    }
}

fn parse_expr(parser: &mut Parser<'_>) -> Result<(), ParseError> {
    let cp = parser.checkpoint();
    skip_whitespace(parser);

    let has_parens = parser.current() == SyntaxKind::L_PAREN;
    if has_parens {
        parser.bump();
        skip_whitespace(parser);
    }

    let kind = parser.current();
    match kind {
        SyntaxKind::IDENTIFIER
        | SyntaxKind::STRING_LIT
        | SyntaxKind::NUMBER_LIT
        | SyntaxKind::TRUE_KW
        | SyntaxKind::FALSE_KW
        | SyntaxKind::UNDEF_KW => {
            parse_term(parser, cp)?;
        }
        SyntaxKind::L_PAREN => {
            todo!("Handle nested parens");
        }
        _ => return Err(ParseError),
    }

    if has_parens {
        let mut parser = parser.start_node_at(cp, SyntaxKind::EXPR);
        parser.expect(SyntaxKind::R_PAREN)?;
    }

    Ok(())
}

fn parse_term(parser: &mut Parser<'_>, cp: Checkpoint) -> Result<(), ParseError> {
    parse_factor(parser, cp)?;
    skip_whitespace(parser);

    match parser.current() {
        SyntaxKind::PLUS | SyntaxKind::MINUS => {
            let mut parser = parser.start_node_at(cp, SyntaxKind::EXPR);
            let kind = parser.current();
            parse_terminal_node(&mut parser, kind)?;
            parse_expr(&mut parser)?;
        }
        _ => {}
    }

    Ok(())
}

fn parse_factor(parser: &mut Parser<'_>, cp: Checkpoint) -> Result<(), ParseError> {
    parse_atom(parser, cp)?;
    skip_whitespace(parser);

    match parser.current() {
        SyntaxKind::ASTERISK | SyntaxKind::SLASH => {
            let mut parser = parser.start_node_at(cp, SyntaxKind::EXPR);
            let kind = parser.current();
            parse_terminal_node(&mut parser, kind)?;
            parse_expr(&mut parser)?;
        }
        _ => {}
    }

    Ok(())
}

fn parse_atom(parser: &mut Parser<'_>, cp: Checkpoint) -> Result<(), ParseError> {
    let mut parser = parser.start_node_at(cp, SyntaxKind::EXPR);

    match parser.current() {
        SyntaxKind::IDENTIFIER => todo!(),
        kind @ (SyntaxKind::STRING_LIT
        | SyntaxKind::NUMBER_LIT
        | SyntaxKind::TRUE_KW
        | SyntaxKind::FALSE_KW
        | SyntaxKind::UNDEF_KW) => parse_terminal_node(&mut parser, kind)?,
        _ => todo!(),
    }

    Ok(())
}

fn parse_terminal_node(parser: &mut Parser<'_>, kind: SyntaxKind) -> Result<(), ParseError> {
    let mut parser = parser.start_node(kind);
    parser.expect(kind)?;
    Ok(())
}

fn parse_statement(parser: &mut Parser<'_>) -> Result<(), ParseError> {
    let mut parser = parser.start_node(SyntaxKind::STATEMENT);
    skip_whitespace(&mut parser);

    match parser.current() {
        SyntaxKind::IDENTIFIER => {
            parse_assignment(&mut parser)?;
            parser.expect(SyntaxKind::SEMICOLON)?;
            Ok(())
        }
        SyntaxKind::INCLUDE_KW => todo!(),
        SyntaxKind::USE_KW => todo!(),
        SyntaxKind::FUNCTION_KW => todo!(),
        SyntaxKind::MODULE_KW => todo!(),
        _ => Err(ParseError),
    }
}

fn parse_package(parser: &mut Parser<'_>) {
    let mut parser = parser.start_node(SyntaxKind::PACKAGE);

    while parser.current() != SyntaxKind::EOF {
        let cp = parser.builder.checkpoint();

        if let Err(_) = parse_statement(&mut parser) {
            // Fast-forward until the next "safe" point so we can continue
            parser.builder.start_node_at(cp, SyntaxKind::ERROR.into());
            let terminals = [SyntaxKind::EOF, SyntaxKind::SEMICOLON];
            while !terminals.contains(&parser.current()) {
                parser.bump();
            }
            // Make sure we step past the terminal
            parser.bump();
            parser.builder.finish_node();
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ParseError;

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(test)]
    mod tests {
        use super::*;

        macro_rules! parse_test {
            ($(#[$meta:meta])* $name:ident, $src:expr, $parse:expr, $assertions:expr) => {
                #[test]
                $(#[$meta])*
                fn $name() {
                    let mut parser = Parser::new($src);

                    let p: fn(&mut Parser) = $parse;
                    p(&mut parser);

                    let value = parser.finish();

                    println!("{value:#?}");
                    $assertions(&value);

                    for node in value.syntax().descendants() {
                        if node.green().kind() == SyntaxKind::ERROR {
                            panic!("Error node {node:#?}");
                        }
                    }
                }
            };
        }

        parse_test!(
            parse_basic_assignment,
            "x = 42",
            |p| parse_assignment(p).unwrap(),
            |a: &Assignment| {
                assert_eq!(a.identifier().unwrap().name(), "x");
                assert_eq!(a.expression().unwrap().number().unwrap().literal(), "42");
            }
        );

        parse_test!(
            parse_true,
            "true",
            |p| parse_expr(p).unwrap(),
            |expr: &Expr| {
                let b = expr.boolean().unwrap();
                assert_eq!(b.value(), true);
            }
        );

        parse_test!(
            parse_false,
            "false",
            |p| parse_expr(p).unwrap(),
            |expr: &Expr| {
                let b = expr.boolean().unwrap();
                assert_eq!(b.value(), false);
            }
        );

        parse_test!(
            parse_string_literal,
            r#""Hello, World!""#,
            |p| parse_expr(p).unwrap(),
            |expr: &Expr| {
                let s = expr.string().unwrap();
                assert_eq!(s.value(), "Hello, World!");
            }
        );

        parse_test!(
            parse_undef,
            "undef",
            |p| parse_expr(p).unwrap(),
            |expr: &Expr| {
                let _ = expr.undef().unwrap();
            }
        );

        parse_test!(
            parse_addition,
            "1 + 2",
            |p| parse_expr(p).unwrap(),
            |expr: &Expr| {
                let (left, op, right) = expr.binary_op().unwrap();
                assert_eq!(left.number().unwrap().literal(), "1");
                assert_eq!(op.kind(), SyntaxKind::PLUS);
                assert_eq!(right.number().unwrap().literal(), "2");
            }
        );

        parse_test!(
            #[ignore = "Parentheses are broken"]
            parse_expr_in_parens,
            "(true)",
            |p| parse_expr(p).unwrap(),
            |expr: &Expr| {
                let b = expr.boolean().unwrap();
                assert_eq!(b.value(), true);
            }
        );

        parse_test!(
            package_with_one_assignment,
            "x = 42;",
            |p| parse_package(p),
            |pkg: &Package| {
                let statements: Vec<_> = pkg.statements().collect();
                assert_eq!(statements.len(), 1);
                let stmt = &statements[0];
                let assignment = stmt.as_assignment().unwrap();
                assert_eq!(assignment.identifier().unwrap().name(), "x");
            }
        );

        parse_test!(
            package_with_multiple_assignments,
            "x = 42; y = 1; z = 2;",
            |p| parse_package(p),
            |pkg: &Package| {
                let statements: Vec<_> = pkg.statements().collect();
                assert_eq!(statements.len(), 3);
                assert!(statements.iter().all(|stmt| stmt.as_assignment().is_some()));
            }
        );
    }
}
