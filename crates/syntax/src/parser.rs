use std::panic::Location;

use drop_bomb::DropBomb;
use rowan::{Checkpoint, GreenNodeBuilder, TextRange, TextSize};

use crate::{
    grammar::{ParseError, TokenSet},
    SyntaxKind, SyntaxNode,
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
    pub(crate) fn finish(self) -> (SyntaxNode, Vec<ParseError>) {
        let Parser {
            builder, errors, ..
        } = self;

        (SyntaxNode::new_root(builder.finish()), errors)
    }

    /// Returns the kind of the current token.
    ///
    /// If parser has already reached the end of input, [`SyntaxKind::EOF`]
    /// is returned.
    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Lookahead operation: returns the kind of the next nth token.
    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        assert!(
            n <= 1,
            "You can't look ahead more than 1 token in a LL(1) grammar"
        );
        self.input.kind(self.position + n)
    }

    /// Checks if the current token is `kind`.
    pub(crate) fn at(&self, kind: impl Into<TokenSet>) -> bool {
        self.nth_at(0, kind)
    }

    /// Look ahead and see if a future token is what we expect.
    pub(crate) fn nth_at(&self, lookahead: usize, kind: impl Into<TokenSet>) -> bool {
        let token_set = kind.into();
        token_set.contains(self.nth(lookahead))
    }

    /// Try to consume a particular type of token.
    pub(crate) fn eat(&mut self, kind: impl Into<TokenSet>) -> bool {
        if !self.at(kind) {
            return false;
        }

        self.do_bump(1);
        true
    }

    /// Assert that we are at a particular token and consume it, panicking if
    /// it wasn't found.
    #[track_caller]
    pub(crate) fn bump(&mut self, kind: impl Into<TokenSet>) {
        let kind = kind.into();

        assert!(
            self.eat(kind),
            "Expected {kind} at position {}",
            self.position
        );
    }

    /// [Eat][Parser::eat] a particular token, emitting an
    /// [`error`][Parser::error] if it wasn't found.
    pub(crate) fn expect(&mut self, kind: impl Into<TokenSet>) {
        let kind = kind.into();

        if !self.eat(kind) {
            self.error(format!("Expected {kind}"));
        }
    }

    /// Unconditionally consume a specified number of tokens.
    pub(crate) fn do_bump(&mut self, tokens: usize) {
        for _ in 0..tokens {
            let group = self
                .input
                .at(self.position)
                .expect("Can't read past the end of input");

            for (kind, text) in group.leading_trivia.iter().copied() {
                self.builder.token(kind.into(), text);
            }
            self.builder.token(group.kind.into(), group.text);
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

    pub(crate) fn cancel(&self, mut mark: Mark) {
        mark.bomb.defuse();
    }
}

/// A wrapper around a list of `(`[`SyntaxKind`]`, &str)` pairs which gives
/// us some useful methods.
#[derive(Debug)]
struct Input<'a>(Vec<TokenGroup<'a>>);

impl<'a> Input<'a> {
    fn new(tokens: impl IntoIterator<Item = (SyntaxKind, &'a str)>) -> Self {
        let mut trivia = Vec::new();
        let mut groups: Vec<TokenGroup<'_>> = Vec::new();
        let mut length_so_far = TextSize::default();

        for (kind, text) in tokens {
            length_so_far += TextSize::of(text);

            match kind {
                SyntaxKind::COMMENT | SyntaxKind::WHITESPACE => {
                    trivia.push((kind, text));
                }
                _ => {
                    let start = groups.last().map(|t| t.location.end()).unwrap_or_default();
                    groups.push(TokenGroup {
                        kind,
                        text,
                        leading_trivia: std::mem::take(&mut trivia),
                        location: TextRange::new(start, length_so_far),
                    });
                }
            }
        }

        // Note: We deliberately ignore any trivia left at the end of the file.
        // Technically it means our trees won't contain trailing comments or
        // whitespace, but we aren't implementing an IDE so it should be fine.
        //
        // The compiler won't care.

        Input(groups)
    }

    fn at(&self, index: usize) -> Option<&TokenGroup<'a>> {
        self.0.get(index)
    }

    fn kind(&self, index: usize) -> SyntaxKind {
        self.at(index).map(|g| g.kind).unwrap_or(SyntaxKind::EOF)
    }

    /// Get the [`TextRange`] which specifies the item at a particular location.
    fn span(&self, index: usize) -> Option<TextRange> {
        self.0.get(index).map(|g| g.location)
    }
}

#[derive(Debug)]
struct TokenGroup<'a> {
    kind: SyntaxKind,
    text: &'a str,
    leading_trivia: Vec<(SyntaxKind, &'a str)>,
    location: TextRange,
}

/// An opaque indicator for a particular place in the token stream.
///
/// A [`Mark`] **must** be consumed by the parser, either using
/// [`Parser::error_recover()`], [`Parser::complete()`], or
/// [`Parser::cancel()`].
#[derive(Debug)]
pub(crate) struct Mark {
    checkpoint: Checkpoint,
    bomb: DropBomb,
}
