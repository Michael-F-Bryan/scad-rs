pub use SyntaxKind::*;

impl From<rowan::SyntaxKind> for SyntaxKind {
    fn from(k: rowan::SyntaxKind) -> Self {
        SyntaxKind::from_u16(k.0).unwrap()
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(k: SyntaxKind) -> Self {
        rowan::SyntaxKind(k.into())
    }
}

impl From<SyntaxKind> for u16 {
    fn from(k: SyntaxKind) -> Self {
        k as u16
    }
}

impl From<SyntaxKind> for m_lexer::TokenKind {
    fn from(k: SyntaxKind) -> m_lexer::TokenKind {
        m_lexer::TokenKind(k.into())
    }
}

macro_rules! as_str {
    ($id:ident) => {
        stringify!($id)
    };
    (-) => {
        "-"
    };
    ($lit:literal) => {
        $lit
    };
    ($other:tt) => {
        stringify!($other)
    };
}

macro_rules! generate_syntax_kind {
    (
        punctuation: { $( $punctuation:tt => $punctuation_name:ident ),* $(,)? },
        keywords: { $( $keyword:tt => $keyword_name:ident ),* $(,)? },
        literals: [ $( $literal_name:ident ),* $(,)? ],
        internal_nodes: [ $( $internal_name:ident ),* $(,)? ],
    ) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        #[repr(u16)]
        #[allow(non_camel_case_types)]
        pub enum SyntaxKind {
            COMMENT,
            WHITESPACE,
            LITERAL,
            IDENTIFIER,
            ERROR,
            EOF,

            $(
                #[doc = concat!("The `", as_str!($punctuation), "` symbol.")]
                $punctuation_name,
            )*

            $(
                #[doc = concat!("The `", as_str!($keyword), "` keyword.")]
                $keyword_name,
            )*

            $($literal_name,)*
            $($internal_name,)*
        }

        impl SyntaxKind {
            const VARIANTS: &[SyntaxKind] = &[
                SyntaxKind::COMMENT,
                SyntaxKind::WHITESPACE,
                SyntaxKind::LITERAL,
                SyntaxKind::IDENTIFIER,
                SyntaxKind::ERROR,
                SyntaxKind::EOF,
                $(SyntaxKind::$punctuation_name,)*
                $(SyntaxKind::$keyword_name,)*
                $(SyntaxKind::$literal_name,)*
                $(SyntaxKind::$internal_name,)*
            ];

            pub const fn is_punctuation(self) -> bool {
                match self {
                    $( SyntaxKind::$punctuation_name )|* => true,
                    _ => false,
                }
            }

            pub const fn is_keyword(self) -> bool {
                match self {
                    $( SyntaxKind::$keyword_name )|* => true,
                    _ => false,
                }
            }

            pub const fn is_literal(self) -> bool {
                match self {
                    $( SyntaxKind::$literal_name )|* => true,
                    _ => false,
                }
            }

            pub const fn is_internal_node(self) -> bool {
                match self {
                    $( SyntaxKind::$internal_name )|* => true,
                    _ => false,
                }
            }

            pub const fn from_u16(n: u16) -> Option<Self> {
                let n = n as usize;

                if n < SyntaxKind::VARIANTS.len() {
                    Some(SyntaxKind::VARIANTS[n])
                } else {
                    None
                }
            }

            pub const fn symbol(self) -> Option<&'static str> {
                match self {
                    $( SyntaxKind::$keyword_name => Some(stringify!($keyword)), )*
                    $( SyntaxKind::$punctuation_name => Some(stringify!($punctuation)), )*
                    _ => None,
                }
            }

            pub fn from_symbol(symbol: &str) -> Option<SyntaxKind> {
                match symbol {
                    $( as_str!($keyword) => Some(SyntaxKind::$keyword_name), )*
                    $( as_str!($punctuation) => Some(SyntaxKind::$punctuation_name), )*
                    _ => None,
                }
            }
        }

        /// Create a [`SyntaxKind`] based on its textual representation.
        #[macro_export]
        macro_rules! T {
            $(
                ($punctuation) => { $crate::syntax_kind::$punctuation_name };
            )*
            $(
                ($keyword) => { $crate::syntax_kind::$keyword_name };
            )*
        }
    };
}

generate_syntax_kind! {
    punctuation: {
        + => PLUS,
        - => MINUS,
        * => STAR,
        / => SLASH,
        ^ => EXPONENT,
        # => HASH,
        % => PERCENT,
        = => EQUAL,
        ! => BANG,
        && => AND,
        || => OR,
        : => COLON,
        ; => SEMICOLON,
        , => COMMA,
        . => DOT,
        ? => QUESTION_MARK,
        "(" => L_PAREN,
        ")" => R_PAREN,
        "{" => L_CURLY,
        "}" => R_CURLY,
        "[" => L_BRACKET,
        "]" => R_BRACKET,
        < => LESS_THAN,
        > => GREATER_THAN,
        <= => LESS_THAN_EQ,
        >= => GREATER_THAN_EQ,
    },
    keywords: {
        false => FALSE_KW,
        for => FOR_KW,
        function => FUNCTION_KW,
        if => IF_KW,
        include => INCLUDE_KW,
        let => LET_KW,
        module => MODULE_KW,
        true => TRUE_KW,
        undef => UNDEF_KW,
        use => USE_KW,
    },
    literals: [
        FLOAT_LIT,
        INTEGER_LIT,
        STRING_LIT,
    ],
    internal_nodes: [
        PACKAGE,
    ],
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn variants_have_the_correct_indices() {
        for (i, &variant) in SyntaxKind::VARIANTS.iter().enumerate() {
            let integer_value = variant as usize;
            assert_eq!(i, integer_value, "{variant:?}");
        }
    }
}
