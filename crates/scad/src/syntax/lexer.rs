use m_lexer::{Lexer, LexerBuilder};
use num_traits::FromPrimitive;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct OpenSCAD;

impl rowan::Language for OpenSCAD {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// The different types of terminals and nodes that can be found in the OpenSCAD
/// grammar.
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
    // Punctuation
    AND,
    ASTERISK,
    BANG,
    R_CURLY,
    R_PAREN,
    R_BRACKET,
    COLON,
    COMMA,
    DOT,
    EQUALS,
    EXPONENT,
    GREATER_THAN_EQUAL,
    GREATER_THAN,
    HASH,
    IDENTIFIER,
    LESS_THAN_EQUAL,
    LESS_THAN,
    MINUS,
    L_CURLY,
    L_PAREN,
    L_BRACKET,
    OR,
    PERCENT,
    PLUS,
    QUESTION_MARK,
    SEMICOLON,
    SLASH,

    // literals
    NUMBER_LIT,
    STRING_LIT,

    // keywords
    FALSE_KW,
    FOR_KW,
    FUNCTION_KW,
    MODULE_KW,
    USE_KW,
    TRUE_KW,
    UNDEF_KW,
    LET_KW,
    IF_KW,
    INCLUDE_KW,

    // Special tokens
    /// Unknown input.
    INVALID_TOKEN,
    ERROR,
    WHITESPACE,
    COMMENT,
    EOF,

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

impl PartialEq<rowan::SyntaxKind> for SyntaxKind {
    fn eq(&self, other: &rowan::SyntaxKind) -> bool {
        *self as u16 == other.0
    }
}

impl PartialEq<SyntaxKind> for rowan::SyntaxKind {
    fn eq(&self, other: &SyntaxKind) -> bool {
        other == self
    }
}

/// Split the input text into its tokens.
///
/// Unknown input will be preserved as an [`UNKNOWN`] token.
pub fn tokenize(input: &str) -> impl Iterator<Item = (SyntaxKind, &'_ str)> {
    static LEXER: Lazy<Lexer> = Lazy::new(lexer);

    let mut start_index = 0;

    LEXER.tokenize(input).into_iter().map(move |tok| {
        let end = start_index + tok.len;
        let text = &input[start_index..end];
        start_index = end;

        (SyntaxKind::from_u16(tok.kind.0).unwrap(), text)
    })
}

fn lexer() -> Lexer {
    LexerBuilder::new()
        .error_token(INVALID_TOKEN.into())
        .token(AND.into(), "&&")
        .token(ASTERISK.into(), r"\*")
        .token(BANG.into(), "!")
        .token(R_CURLY.into(), r"\}")
        .token(R_PAREN.into(), r"\)")
        .token(R_BRACKET.into(), r"\]")
        .token(COLON.into(), ":")
        .token(COMMA.into(), ",")
        .token(COMMENT.into(), r"//[^\n]*")
        .external_token(COMMENT.into(), r"/\*", block_comment)
        .token(DOT.into(), r"\.")
        .token(EQUALS.into(), "=")
        .token(EXPONENT.into(), r"\^")
        .token(FALSE_KW.into(), "false")
        .token(FOR_KW.into(), "for")
        .token(FUNCTION_KW.into(), "function")
        .token(MODULE_KW.into(), "module")
        .token(GREATER_THAN_EQUAL.into(), ">=")
        .token(GREATER_THAN.into(), ">")
        .token(HASH.into(), "#")
        .token(IF_KW.into(), "if")
        .token(INCLUDE_KW.into(), "include")
        .token(LESS_THAN_EQUAL.into(), "<=")
        .token(LESS_THAN.into(), "<")
        .token(LET_KW.into(), "let")
        .token(MINUS.into(), "-")
        // https://stackoverflow.com/a/55592455
        .token(
            NUMBER_LIT.into(),
            r"\d+([.]\d*)?([eE][+-]?\d+)?|[.]\d+([eE][+-]?\d+)?",
        )
        .token(L_CURLY.into(), r"\{")
        .token(L_PAREN.into(), r"\(")
        .token(L_BRACKET.into(), r"\[")
        .token(OR.into(), r"\|\|")
        .token(PERCENT.into(), "%")
        .token(PLUS.into(), r"\+")
        .token(QUESTION_MARK.into(), r"\?")
        .token(SEMICOLON.into(), ";")
        .token(SLASH.into(), "/")
        // https://wordaligned.org/articles/string-literals-and-regular-expressions
        .token(STRING_LIT.into(), r#""([^"\\]|\\.)*""#)
        .token(TRUE_KW.into(), "true")
        .token(USE_KW.into(), "use")
        .token(UNDEF_KW.into(), "undef")
        .token(WHITESPACE.into(), r"\s+")
        // Note: push this to the bottom so it's the lowest precedence
        .token(IDENTIFIER.into(), r"[\$\p{Alphabetic}][\w_]*")
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

        let tokens: Vec<_> = tokenize(src).collect();

        let round_tripped: String = tokens.iter().map(|(_, text)| *text).collect();
        assert_eq!(round_tripped, src);
        tokens
            .iter()
            .copied()
            .for_each(|(kind, text)| assert_ne!(kind, ERROR, "Invalid token: {text:?}"));
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn block_comments() {
        let src = "/* This is a
            block comment*/";
        let expected = vec![(SyntaxKind::COMMENT, src)];

        let tokens: Vec<_> = tokenize(src).collect();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn different_number_formats() {
        let inputs = ["4", "31", "3.1", "3.", ".3", "3.14e0"];

        for input in inputs {
            let tokens: Vec<_> = tokenize(input).collect();

            let expected = vec![(SyntaxKind::NUMBER_LIT, input)];
            assert_eq!(tokens, expected);
        }
    }
}
