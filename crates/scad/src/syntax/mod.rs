//! A lexer for the SCAD programming language.
//!
//! See the [grammar file][grammar] for more.
//!
//! [grammar]: https://files.openscad.org/grammar.xhtml

use m_lexer::{Lexer, LexerBuilder};
use num_traits::FromPrimitive;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Scad;

impl rowan::Language for Scad {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    num_derive::FromPrimitive,
    num_derive::ToPrimitive,
)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    AND,
    ASTERISK,
    BANG,
    CLOSE_CURLY,
    CLOSE_PAREN,
    CLOSE_SQUARE,
    COLON,
    COMMA,
    DOT,
    EQUALS,
    EXPONENT,
    FALSE,
    FOR,
    FUNCTION,
    GREATER_THAN_EQUAL,
    GREATER_THAN,
    HASH,
    IDENTIFIER,
    IF,
    INCLUDE_KW,
    LESS_THAN_EQUAL,
    LESS_THAN,
    LET,
    MINUS,
    NUMBER,
    OPEN_CURLY,
    OPEN_PAREN,
    OPEN_SQUARE,
    OR,
    PERCENT,
    PLUS,
    QUESTION_MARK,
    SEMICOLON,
    SLASH,
    STRING,
    TRUE,
    UNDEF,
    USE_KW,

    // Special tokens
    /// Unknown input.
    UNKNOWN,
    WHITESPACE,
    COMMENT,

    // Composite nodes
    PACKAGE,
    STATEMENT,
    INCLUDE,
    USE,
    EXPR,
    LOOKUP,
    RANGE_EXPRESSION,
    LIST_EXPRESSION,
    LIST_COMPREHENSION_ELEMENTS,
    LET_CLAUSE,
    FOR_CLAUSE,
    IF_CLAUSE,
    NAMED_FUNCTION_DEFINITION,
    NAMED_MODULE_DEFINITION,
    FUNCTION_CALL,
    MODULE_INSTANTIATION,
    CHILD,
    PARAMETERS,
    PARAMETER,
    ARGUMENTS,
    ARGUMENT,
    ASSIGNMENTS,
    ASSIGNMENT,
}

use once_cell::sync::Lazy;
pub use SyntaxKind::*;

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}

impl From<SyntaxKind> for u16 {
    fn from(kind: SyntaxKind) -> u16 {
        kind as u16
    }
}

impl From<SyntaxKind> for m_lexer::TokenKind {
    fn from(kind: SyntaxKind) -> m_lexer::TokenKind {
        m_lexer::TokenKind(kind.into())
    }
}

/// Split the input text into its tokens.
///
/// Unknown input will be preserved as an [`UNKNOWN`] token.
pub fn tokenize(input: &str) -> Vec<(SyntaxKind, &'_ str)> {
    static LEXER: Lazy<Lexer> = Lazy::new(lexer);

    let mut start_index = 0;

    LEXER
        .tokenize(input)
        .into_iter()
        .map(|tok| {
            let end = start_index + tok.len;
            let text = &input[start_index..end];
            start_index = end;

            (SyntaxKind::from_u16(tok.kind.0).unwrap(), text)
        })
        .collect()
}

fn lexer() -> Lexer {
    LexerBuilder::new()
        .error_token(UNKNOWN.into())
        .token(AND.into(), "&&")
        .token(ASTERISK.into(), r"\*")
        .token(BANG.into(), "!")
        .token(CLOSE_CURLY.into(), r"\}")
        .token(CLOSE_PAREN.into(), r"\)")
        .token(CLOSE_SQUARE.into(), r"\]")
        .token(COLON.into(), ":")
        .token(COMMA.into(), ",")
        .token(COMMENT.into(), r"//[^\n]*")
        .external_token(COMMENT.into(), r"/\*", block_comment)
        .token(DOT.into(), r"\.")
        .token(EQUALS.into(), "=")
        .token(EXPONENT.into(), r"\^")
        .token(FALSE.into(), "false")
        .token(FOR.into(), "for")
        .token(FUNCTION.into(), "function")
        .token(GREATER_THAN_EQUAL.into(), ">=")
        .token(GREATER_THAN.into(), ">")
        .token(HASH.into(), "#")
        .token(IDENTIFIER.into(), r"\$?\w[\w\d]*")
        .token(IF.into(), "if")
        .token(INCLUDE_KW.into(), "include")
        .token(LESS_THAN_EQUAL.into(), "<=")
        .token(LESS_THAN.into(), "<")
        .token(LET.into(), "let")
        .token(MINUS.into(), "-")
        // https://stackoverflow.com/a/55592455
        .token(
            NUMBER.into(),
            r"\d+([.]\d*)?([eE][+-]?\d+)?|[.]\d+([eE][+-]?\d+)?",
        )
        .token(OPEN_CURLY.into(), r"\{")
        .token(OPEN_PAREN.into(), r"\(")
        .token(OPEN_SQUARE.into(), r"\[")
        .token(OR.into(), r"\|\|")
        .token(PERCENT.into(), "%")
        .token(PLUS.into(), r"\+")
        .token(QUESTION_MARK.into(), r"\?")
        .token(SEMICOLON.into(), ";")
        .token(SLASH.into(), "/")
        // https://wordaligned.org/articles/string-literals-and-regular-expressions
        .token(STRING.into(), r#""([^"\\]|\\.)*""#)
        .token(TRUE.into(), "true")
        .token(UNDEF.into(), "undef")
        .token(USE_KW.into(), "use")
        .token(WHITESPACE.into(), r"\s+")
        .build()
}

fn block_comment(text: &str) -> Option<usize> {
    debug_assert!(text.starts_with("/*"));
    let end_token = "*/";
    let len = text.find(end_token)?;
    Some(len + end_token.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_program() {
        // https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Transformations#rotate
        let src = "
            x= 10; y = 10; z = 10; // point coordinates of end of cylinder

            length = norm([x,y,z]);  // radial distance
            b = acos(z/length); // inclination angle
            c = atan2(y,x);     // azimuthal angle

            rotate([0, b, c])
                cylinder(h=length, r=0.5);
            %cube([x,y,z]); // corner of cube should coincide with end of cylinder
        ";

        let tokens = tokenize(src);

        let round_tripped: String = tokens.iter().map(|(_, text)| *text).collect();
        assert_eq!(round_tripped, src);
        tokens
            .iter()
            .copied()
            .for_each(|(kind, text)| assert_ne!(kind, UNKNOWN, "Invalid token: {text:?}"));
        insta::assert_debug_snapshot!(tokens);
    }
}
