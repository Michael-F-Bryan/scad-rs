use crate::{SyntaxKind, SyntaxKind::*};
use m_lexer::{Lexer, LexerBuilder};

/// Split the input text into its tokens.
///
/// Unknown input will be preserved as an [`ERROR`] token.
pub fn tokenize(input: &str) -> impl Iterator<Item = (SyntaxKind, &'_ str)> {
    let mut start_index = 0;

    lexer().tokenize(input).into_iter().map(move |tok| {
        let end = start_index + tok.len;
        let text = &input[start_index..end];
        start_index = end;

        (SyntaxKind::from_code(tok.kind.0).unwrap(), text)
    })
}

fn lexer() -> Lexer {
    LexerBuilder::new()
        .error_token(ERROR.into())
        .token(AND.into(), "&&")
        .token(STAR.into(), r"\*")
        .token(BANG.into(), "!")
        .token(R_CURLY.into(), r"\}")
        .token(R_PAREN.into(), r"\)")
        .token(R_BRACKET.into(), r"\]")
        .token(COLON.into(), ":")
        .token(COMMA.into(), ",")
        .token(COMMENT.into(), r"//[^\n]*")
        .external_token(COMMENT.into(), r"/\*", block_comment)
        .token(EQUALS.into(), "=")
        // TODO: add these to the ungrammar
        // .token(DOT.into(), r"\.")
        // .token(EXPONENT.into(), r"\^")
        // .token(HASH.into(), "#")
        .token(FALSE_KW.into(), "false")
        .token(FOR_KW.into(), "for")
        .token(FUNCTION_KW.into(), "function")
        .token(MODULE_KW.into(), "module")
        .token(GREATER_THAN_EQUALS.into(), ">=")
        .token(GREATER_THAN.into(), ">")
        .token(IF_KW.into(), "if")
        .token(INCLUDE_KW.into(), "include")
        .token(LESS_THAN_EQUALS.into(), "<=")
        .token(LESS_THAN.into(), "<")
        .token(LET_KW.into(), "let")
        .token(MINUS.into(), "-")
        .token(INTEGER.into(), r"\d+")
        // https://stackoverflow.com/a/55592455
        .token(
            FLOAT.into(),
            r"\d+([.]\d*)?([eE][+-]?\d+)?|[.]\d+([eE][+-]?\d+)?",
        )
        .token(INTEGER.into(), r"\d+")
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
        .token(STRING.into(), r#""([^"\\]|\\.)*""#)
        .token(TRUE_KW.into(), "true")
        .token(USE_KW.into(), "use")
        .token(UNDEF_KW.into(), "undef")
        .token(WHITESPACE.into(), r"\s+")
        // Note: push this to the bottom so it's the lowest precedence
        .token(IDENT.into(), r"[\$\p{Alphabetic}][\w_]*")
        .build()
}

fn block_comment(text: &str) -> Option<usize> {
    debug_assert!(text.starts_with("/*"));

    let mut nesting = 0;
    let mut state = State::Scanning;

    enum State {
        Scanning,
        ReadingStart,
        ReadingEnd,
    }

    for (offset, c) in text.char_indices() {
        match state {
            State::Scanning if c == '/' => {
                state = State::ReadingStart;
            }
            State::Scanning if c == '*' => {
                state = State::ReadingEnd;
            }
            State::ReadingEnd if c == '/' => {
                nesting -= 1;
                if nesting == 0 {
                    return Some(offset + c.len_utf8());
                }
                state = State::Scanning;
            }
            State::ReadingStart if c == '*' => {
                nesting += 1;
                state = State::Scanning;
            }
            _ => {}
        }
    }

    // We've encountered an unterminated block comment. Let's let the lexer try
    // something else or return an error
    None
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
    fn block_comments_with_nesting() {
        let src = "/* This is a /* nested */
            block comment*/";
        let expected = vec![(SyntaxKind::COMMENT, src)];

        let tokens: Vec<_> = tokenize(src).collect();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn different_number_formats() {
        let inputs = ["4", "31", "3.1", "3.", ".3", "3.14e0"];
        let expected = [INTEGER, INTEGER, FLOAT, FLOAT, FLOAT, FLOAT];

        for (input, expected) in inputs.into_iter().zip(expected) {
            let tokens: Vec<_> = tokenize(input).collect();
            let expected = vec![(expected, input)];
            assert_eq!(tokens, expected);
        }
    }
}
