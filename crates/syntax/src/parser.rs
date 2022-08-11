use std::panic::Location;

use drop_bomb::DropBomb;
use rowan::{Checkpoint, GreenNodeBuilder, SyntaxNode, TextRange, TextSize};

use crate::{
    grammar::{ParseError, TokenSet},
    OpenSCAD, SyntaxKind,
};

#[derive(Debug)]
pub(crate) struct Parser<'a> {
    input: Input<'a>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,
    position: usize,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: impl IntoIterator<Item = (SyntaxKind, &'a str)>) -> Self {
        Parser {
            input: Input::new(tokens),
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
            position: 0,
        }
    }

    /// Finish parsing, yielding the final [`SyntaxNode`] and any errors that
    /// were found.
    pub(crate) fn finish(self) -> (SyntaxNode<OpenSCAD>, Vec<ParseError>) {
        let Parser {
            builder, errors, ..
        } = self;

        (SyntaxNode::new_root(builder.finish()), errors)
    }

    /// Returns the kind of the current token.
    /// If parser has already reached the end of input,
    /// the special [`SyntaxKind::EOF`] kind is returned.
    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Lookahead operation: returns the kind of the next nth
    /// token.
    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        assert!(
            n <= 1,
            "You can't look ahead more than 1 token in a LL(1) grammar"
        );
        self.input.kind(self.position + n)
    }

    /// Checks if the current token is `kind`.
    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    /// Look ahead and see if a future token is what we expect.
    pub(crate) fn nth_at(&self, lookahead: usize, kind: SyntaxKind) -> bool {
        self.nth(lookahead) == kind
    }

    /// Try to consume a particular type of token.
    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }

        self.do_bump(1);
        true
    }

    /// Assert that we are at a particular token and consume it, panicking if
    /// it wasn't found.
    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert!(
            self.eat(kind),
            "Expected a {kind:?} at position {}",
            self.position
        );
    }

    /// [Eat][Parser::eat] a particular token, emitting an
    /// [`error`][Parser::error] if it wasn't found.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) {
        if !self.eat(kind) {
            self.error(format!("Expected {kind:?}"));
        }
    }

    /// Unconditionally consume a specified number of tokens.
    pub(crate) fn do_bump(&mut self, tokens: usize) {
        for _ in 0..tokens {
            let (kind, text) = self.input.at(self.position);
            self.builder.token(kind.into(), text);
            self.position += 1;
        }
    }

    /// Create a [`Mark`] that can be used to record a node.
    #[track_caller]
    pub(crate) fn start(&mut self) -> Mark {
        let caller = Location::caller().to_string();

        Mark {
            checkpoint: self.builder.checkpoint(),
            bomb: DropBomb::new(format!("Incomplete node ({caller})")),
        }
    }

    /// Wrap all tokens consumed since the [`Mark`] was created in a node.
    pub(crate) fn complete(&mut self, mark: Mark, kind: SyntaxKind) {
        let Mark {
            checkpoint,
            mut bomb,
        } = mark;
        bomb.defuse();
        self.builder.start_node_at(checkpoint, kind.into());
        self.builder.finish_node();
    }

    /// Record a [`ParseError`] that points at the current token.
    pub(crate) fn error(&mut self, msg: impl Into<String>) {
        let location = self.input.span(self.position).unwrap();
        self.errors.push(ParseError {
            location,
            msg: msg.into(),
        });
    }

    /// Create an error node and keep consuming tokens until we have consumed
    /// something in the recovery set.
    pub(crate) fn error_recover(&mut self, msg: &str, mark: Mark, recovery: impl Into<TokenSet>) {
        self.error(msg);

        let recovery = recovery.into();

        while !recovery.contains(self.current()) && self.current() != SyntaxKind::EOF {
            self.do_bump(1);
        }

        // make sure we  eat the recovery token as well
        self.do_bump(1);
        self.complete(mark, SyntaxKind::ERROR);
    }
}

/// A wrapper around a list of `(`[`SyntaxKind`]`, &str)` pairs which gives
/// us some useful methods.
#[derive(Debug)]
struct Input<'a>(Vec<(SyntaxKind, &'a str)>);

impl<'a> Input<'a> {
    fn new(tokens: impl IntoIterator<Item = (SyntaxKind, &'a str)>) -> Self {
        // FIXME: Blindly skipping whitespace and comments like this means
        // Rowan will never see them, so all our token spans will be wrong.
        let tokens = tokens
            .into_iter()
            .filter(|(k, _)| *k != SyntaxKind::WHITESPACE && *k != SyntaxKind::COMMENT)
            .collect();

        Input(tokens)
    }

    fn at(&self, index: usize) -> (SyntaxKind, &'a str) {
        self.0.get(index).copied().unwrap_or((SyntaxKind::EOF, ""))
    }

    fn kind(&self, index: usize) -> SyntaxKind {
        let (kind, _) = self.at(index);
        kind
    }

    /// Get the [`TextRange`] which specifies the item at a particular location.
    fn span(&self, index: usize) -> Option<TextRange> {
        let previous = self.0.get(..index)?;
        let start_index = previous.iter().copied().map(|(_, t)| TextSize::of(t)).sum();

        let (_, target) = self.0.get(index)?;
        let end_index = start_index + TextSize::of(*target);

        Some(TextRange::new(start_index, end_index))
    }
}

/// An opaque indicator for a particular place in the token stream.
///
/// A [`Mark`] **must** be consumed by the parser, either using
/// [`Parser::error_recover()`] or [`Parser::complete()`].
#[derive(Debug)]
pub(crate) struct Mark {
    checkpoint: Checkpoint,
    bomb: DropBomb,
}
