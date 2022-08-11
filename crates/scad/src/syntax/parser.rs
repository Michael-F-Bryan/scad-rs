use std::{
    collections::VecDeque,
    ops::{Deref, DerefMut},
};

use im::Vector;
use rowan::{ast::AstNode, Checkpoint, GreenNodeBuilder, SyntaxNode};

use crate::{
    ast::*,
    syntax::{lexer::OpenSCAD, SyntaxKind, SyntaxKind::*},
};

pub fn parse(text: &str) -> (Package, Vector<ParseError>) {
    let mut parser = Parser::new(text);
    parse_package(&mut parser);
    let pkg = parser.finish();

    (pkg, Vector::new())
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
            self.token(k, text);
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
        SyntaxKind::IDENTIFIER => {
            parse_ident(&mut parser)?;
            skip_whitespace(&mut parser);

            if parser.current() == SyntaxKind::L_PAREN {
                let mut parser = parser.start_node_at(cp, SyntaxKind::FUNCTION_CALL);
                parse_parameters(&mut parser)?;
            } else {
                let parser = parser.start_node_at(cp, SyntaxKind::LOOKUP);
                // explicitly end the LOOKUP node
                drop(parser);
            }
        }
        kind @ (SyntaxKind::STRING_LIT
        | SyntaxKind::NUMBER_LIT
        | SyntaxKind::TRUE_KW
        | SyntaxKind::FALSE_KW
        | SyntaxKind::UNDEF_KW) => parse_terminal_node(&mut parser, kind)?,
        _ => todo!(),
    }

    Ok(())
}

fn parse_parameters(parser: &mut Parser<'_>) -> Result<(), ParseError> {
    let mut parser = parser.start_node(SyntaxKind::PARAMETERS);
    parser.expect(SyntaxKind::L_PAREN)?;

    loop {
        skip_whitespace(&mut parser);

        match parser.current() {
            SyntaxKind::R_PAREN => break,
            SyntaxKind::EOF => return Err(ParseError),
            _ => parse_parameter(&mut parser)?,
        }

        skip_whitespace(&mut parser);

        match parser.current() {
            SyntaxKind::R_PAREN => break,
            SyntaxKind::COMMA => parser.bump(),
            _ => todo!(),
        }
    }

    parser.expect(SyntaxKind::R_PAREN)?;

    Ok(())
}

fn parse_parameter(parser: &mut Parser<'_>) -> Result<(), ParseError> {
    let mut parser = parser.start_node(SyntaxKind::PARAMETER);
    skip_whitespace(&mut parser);

    if parser.current() == SyntaxKind::IDENTIFIER {
        // we need to look ahead to see whether we are parsing a named argument
        // or positional one
        let next_token = parser
            .tokens
            .iter()
            .map(|(kind, _)| *kind)
            .skip(1)
            .skip_while(|&kind| kind == SyntaxKind::WHITESPACE)
            .next()
            .unwrap_or(SyntaxKind::EOF);

        if next_token == SyntaxKind::EQUALS {
            return parse_assignment(&mut parser);
        }
    }

    parse_expr(&mut parser)
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

        if parse_statement(&mut parser).is_err() {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ParseError;

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Debug;

    struct TestCase<T> {
        assertions: Vec<Box<dyn FnOnce(&T)>>,
        src: String,
    }

    impl<T> TestCase<T>
    where
        T: AstNode<Language = OpenSCAD> + Debug,
    {
        fn new(src: &str) -> Self {
            TestCase {
                assertions: Vec::new(),
                src: src.into(),
            }
        }

        fn assert(mut self, assertion: impl FnOnce(&T) + 'static) -> Self {
            self.assertions.push(Box::new(assertion));
            self
        }

        #[track_caller]
        fn run(self, parse: impl FnOnce(&mut Parser)) {
            let TestCase { assertions, src } = self;
            let mut parser = Parser::new(&src);

            parse(&mut parser);
            let value: T = parser.finish();

            println!("{value:#?}");

            for assertion in assertions {
                assertion(&value);
            }

            for node in value.syntax().descendants() {
                if node.green().kind() == SyntaxKind::ERROR {
                    panic!("Error node {node:#?}");
                }
            }
        }
    }

    #[test]
    fn parse_basic_assignment() {
        TestCase::new("x = 42")
            .assert(|a: &Assignment| {
                assert_eq!(a.identifier().unwrap().name(), "x");
                assert_eq!(a.expression().unwrap().number().unwrap().literal(), "42");
            })
            .run(|p| parse_assignment(p).unwrap());
    }

    #[test]
    fn parse_true() {
        TestCase::new("true")
            .assert(|expr: &Expr| {
                let b = expr.boolean().unwrap();
                assert_eq!(b.value(), true);
            })
            .run(|p| parse_expr(p).unwrap());
    }

    #[test]
    fn parse_false() {
        TestCase::new("false")
            .assert(|expr: &Expr| {
                let b = expr.boolean().unwrap();
                assert_eq!(b.value(), false);
            })
            .run(|p| parse_expr(p).unwrap());
    }

    #[test]
    fn parse_string_literal() {
        TestCase::new(r#""Hello, World!""#)
            .assert(|expr: &Expr| {
                let s = expr.string().unwrap();
                assert_eq!(s.value(), "Hello, World!");
            })
            .run(|p| parse_expr(p).unwrap());
    }

    #[test]
    fn parse_undef() {
        TestCase::new("undef")
            .assert(|expr: &Expr| {
                let _ = expr.undef().unwrap();
            })
            .run(|p| parse_expr(p).unwrap());
    }

    #[test]
    fn parse_addition() {
        TestCase::new("1 + 2")
            .assert(|expr: &Expr| {
                let (left, op, right) = expr.binary_op().unwrap();
                assert_eq!(left.number().unwrap().literal(), "1");
                assert_eq!(op.kind(), SyntaxKind::PLUS);
                assert_eq!(right.number().unwrap().literal(), "2");
            })
            .run(|p| parse_expr(p).unwrap());
    }

    #[test]
    fn parse_variable_reference() {
        TestCase::new("foo")
            .assert(|expr: &Expr| {
                let lookup = expr.lookup().unwrap();
                assert_eq!(lookup.ident().unwrap().name(), "foo");
            })
            .run(|p| parse_expr(p).unwrap());
    }

    #[test]
    fn parse_function_call() {
        TestCase::new("print(\"Hello, World!\", dest = \"stdout\")")
            .assert(|expr: &Expr| {
                let call = expr.function_call().unwrap();

                let ident = call.ident().unwrap();
                assert_eq!(ident.name(), "print");

                let parameters: Vec<_> = call.parameters().unwrap().iter().collect();
                assert_eq!(parameters.len(), 2);

                let first_arg = parameters[0].positional().unwrap();
                assert_eq!(first_arg.string().unwrap().value(), "Hello, World!");

                let second_arg = parameters[1].named().unwrap();
                assert_eq!(second_arg.identifier().unwrap().name(), "dest");
                assert_eq!(
                    second_arg.expression().unwrap().string().unwrap().value(),
                    "stdout"
                );
            })
            .run(|p| parse_expr(p).unwrap());
    }

    #[test]
    #[ignore = "Parentheses are broken"]
    fn parse_expr_in_parens() {
        TestCase::new("(true)")
            .assert(|expr: &Expr| {
                let b = expr.boolean().unwrap();
                assert_eq!(b.value(), true);
            })
            .run(|p| parse_expr(p).unwrap());
    }

    #[test]
    fn package_with_one_assignment() {
        TestCase::new("x = 42;")
            .assert(|pkg: &Package| {
                let statements: Vec<_> = pkg.statements().collect();
                assert_eq!(statements.len(), 1);
                let stmt = &statements[0];
                let assignment = stmt.as_assignment().unwrap();
                assert_eq!(assignment.identifier().unwrap().name(), "x");
            })
            .run(|p| parse_package(p))
    }

    #[test]
    fn package_with_multiple_assignments() {
        TestCase::new("x = 42; y = 1; z = 2;")
            .assert(|pkg: &Package| {
                let statements: Vec<_> = pkg.statements().collect();
                assert_eq!(statements.len(), 3);
                assert!(statements.iter().all(|stmt| stmt.as_assignment().is_some()));
            })
            .run(|p| parse_package(p))
    }
}
