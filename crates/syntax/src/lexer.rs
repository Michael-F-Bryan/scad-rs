use crate::{SyntaxKind, SyntaxKind::*};
use m_lexer::{Lexer, LexerBuilder};

/// Split the input text into its tokens.
///
/// Unknown input will be preserved as an [`ERROR`] token.
pub fn tokenize(input: &str) -> impl Iterator<Item = (SyntaxKind, &'_ str)> {
    let mut start_index = 0;

    let tokens = lexer().tokenize(input);

    tokens.into_iter().map(move |tok| {
        let end = start_index + tok.len;
        let text = &input[start_index..end];
        start_index = end;

        (SyntaxKind::from_code(tok.kind.0).unwrap(), text)
    })
}

fn lexer() -> Lexer {
    let builder = LexerBuilder::new().error_token(ERROR.into());

    // basic tokens
    builder
        // These tokens require some custom logic
        .external_token(COMMENT.into(), r"/\*", block_comment)
        .tokens(&[
            (AND.into(), "&&"),
            (STAR.into(), r"\*"),
            (BANG.into(), "!"),
            (R_CURLY.into(), r"\}"),
            (R_PAREN.into(), r"\)"),
            (R_BRACKET.into(), r"\]"),
            (COLON.into(), ":"),
            (COMMA.into(), ","),
            (COMMENT.into(), r"//[^\n]*"),
            (EQUALS.into(), "="),
            (DOT.into(), r"\."),
            (CARET.into(), r"\^"),
            (HASH.into(), "#"),
            (FALSE_KW.into(), "false"),
            (FOR_KW.into(), "for"),
            (FUNCTION_KW.into(), "function"),
            (MODULE_KW.into(), "module"),
            (GREATER_THAN_EQUALS.into(), ">="),
            (GREATER_THAN.into(), ">"),
            (IF_KW.into(), "if"),
            (INCLUDE_KW.into(), "include"),
            (LESS_THAN_EQUALS.into(), "<="),
            (LESS_THAN.into(), "<"),
            // HACK: To avoid accidentally matching "a < b && b > c", we assume
            // file paths don't contain whitespace. Ideally, we would write our
            // own lexer which has smarts like expecting a file path to contain
            // "/" or "\", but that's more effort than I'd like to invest right
            // now.
            (FILE.into(), r"<\S+>"),
            (LET_KW.into(), "let"),
            (MINUS.into(), "-"),
            (INTEGER.into(), r"\d+"),
            // https://stackoverflow.com/a/55592455
            (
                FLOAT.into(),
                r"\d+([.]\d*)?([eE][+-]?\d+)?|[.]\d+([eE][+-]?\d+)?",
            ),
            (INTEGER.into(), r"\d+"),
            (L_CURLY.into(), r"\{"),
            (L_PAREN.into(), r"\("),
            (L_BRACKET.into(), r"\["),
            (OR.into(), r"\|\|"),
            (PERCENT.into(), "%"),
            (PLUS.into(), r"\+"),
            (QUESTION_MARK.into(), r"\?"),
            (SEMICOLON.into(), ";"),
            (SLASH.into(), "/"),
            // https://wordaligned.org/articles/string-literals-and-regular-expressions
            (STRING.into(), r#""([^"\\]|\\.)*""#),
            (TRUE_KW.into(), "true"),
            (USE_KW.into(), "use"),
            (UNDEF_KW.into(), "undef"),
            (WHITESPACE.into(), r"\s+"),
        ])
        // Note: push this to the bottom so it's the lowest precedence
        .token(IDENT.into(), r"[\$\p{Alphabetic}][\w_]*")
        .build()
}

fn block_comment(text: &str) -> Option<usize> {
    debug_assert!(text.starts_with("/*"));

    let mut nesting = 0;
    let mut state = State::Scanning;

    #[derive(Debug, Copy, Clone)]
    enum State {
        Scanning,
        ReadingStart,
        ReadingEnd,
    }

    for (offset, c) in text.char_indices() {
        dbg!(state, offset, c);
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
            State::ReadingStart => {
                // We previously saw a "/", but the current token isn't a "*"
                // so we should go back to scanning.
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
    fn handle_block_comment_with_internal_slashes() {
        let src = "/* / */";

        let length = block_comment(src).unwrap();

        assert_eq!(length, src.len());
    }

    #[test]
    fn string_literals() {
        let examples = [
            r#""""#,
            r#"" ""#,
            r#""escaped \" quote""#,
            r#""escaped \n newline""#,
            r#""<code>&</code>""#,
        ];

        for example in examples {
            let tokens: Vec<_> = tokenize(example).collect();

            let expected = vec![(SyntaxKind::STRING, example)];
            assert_eq!(tokens, expected)
        }
    }

    #[test]
    fn file_containing_at_cmake() {
        let src = "use <@CMAKE_CURRENT_SOURCE_DIR@/../testdata/scad/misc/sub2/test7.scad>";

        let tokens: Vec<_> = tokenize(src).collect();

        let expected = vec![
            (USE_KW, "use"),
            (WHITESPACE, " "),
            (
                FILE,
                "<@CMAKE_CURRENT_SOURCE_DIR@/../testdata/scad/misc/sub2/test7.scad>",
            ),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn conditionals_arent_recognised_as_a_file() {
        let src = "a < b && c > d";

        let tokens: Vec<_> = tokenize(src).collect();

        let expected = vec![
            (IDENT, "a"),
            (WHITESPACE, " "),
            (LESS_THAN, "<"),
            (WHITESPACE, " "),
            (IDENT, "b"),
            (WHITESPACE, " "),
            (AND, "&&"),
            (WHITESPACE, " "),
            (IDENT, "c"),
            (WHITESPACE, " "),
            (GREATER_THAN, ">"),
            (WHITESPACE, " "),
            (IDENT, "d"),
        ];
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
