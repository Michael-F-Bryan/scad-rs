use heck::ToShoutySnekCase;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

const PUNCTUATION_NAMES: &[(&str, &str)] = &[
    (";", "SEMICOLON"),
    (":", "COLON"),
    (",", "COMMA"),
    (".", "DOT"),
    ("?", "QUESTION_MARK"),
    ("(", "L_PAREN"),
    ("{", "L_CURLY"),
    ("[", "L_BRACKET"),
    (")", "R_PAREN"),
    ("}", "R_CURLY"),
    ("]", "R_BRACKET"),
    ("!", "BANG"),
    ("+", "PLUS"),
    ("-", "MINUS"),
    ("*", "STAR"),
    ("/", "SLASH"),
    ("^", "CARET"),
    ("%", "PERCENT"),
    ("&&", "AND"),
    ("||", "OR"),
    ("<", "LESS_THAN"),
    ("<=", "LESS_THAN_EQUALS"),
    (">", "GREATER_THAN"),
    (">=", "GREATER_THAN_EQUALS"),
    ("=", "EQUALS"),
];

#[derive(Debug, Clone)]
pub struct SyntaxKind {
    keywords: Vec<Keyword>,
    symbols: Vec<Punctuation>,
    special: Vec<SpecialToken>,
    non_terminals: Vec<NonTerminal>,
}

impl SyntaxKind {
    pub fn from_tokens(
        tokens: impl IntoIterator<Item = String>,
        non_terminals: impl IntoIterator<Item = Ident>,
    ) -> Self {
        let (keywords, symbols): (Vec<_>, Vec<_>) = tokens
            .into_iter()
            .partition(|name| name.chars().all(|c| c.is_alphabetic()));

        let special_tokens = vec![
            SpecialToken {
                docs: "An identifier.",
                ident: format_ident!("IDENT"),
            },
            SpecialToken {
                docs: "A lexer error.",
                ident: format_ident!("ERROR"),
            },
            SpecialToken {
                docs: "One or more whitespace characters (spaces, tabs, newlines, etc.).",
                ident: format_ident!("WHITESPACE"),
            },
            SpecialToken {
                docs: "A comment.",
                ident: format_ident!("COMMENT"),
            },
            SpecialToken {
                docs: "An integer literal",
                ident: format_ident!("INTEGER_LIT"),
            },
            SpecialToken {
                docs: "A float literal",
                ident: format_ident!("FLOAT_LIT"),
            },
            SpecialToken {
                docs: "A string literal",
                ident: format_ident!("STRING_LIT"),
            },
        ];

        SyntaxKind {
            keywords: keywords
                .into_iter()
                .filter(|kw| {
                    special_tokens.iter().all(|s| {
                        s.ident.to_string().TO_SHOUTY_SNEK_CASE() != kw.TO_SHOUTY_SNEK_CASE()
                    })
                })
                .map(|word| Keyword {
                    ident: format_ident!("{}_KW", word.TO_SHOUTY_SNEK_CASE()),
                    word,
                })
                .collect(),
            symbols: symbols
                .into_iter()
                .map(|s: String| Punctuation {
                    ident: format_ident!("{}", symbol_name(&s)),
                    symbol: s,
                })
                .collect(),
            special: special_tokens,
            non_terminals: non_terminals.into_iter().map(NonTerminal).collect(),
        }
    }

    fn definition(&self) -> TokenStream {
        let SyntaxKind {
            keywords,
            symbols,
            special,
            non_terminals,
        } = self;

        let symbols = symbols.iter().map(|p| p.variant());
        let special = special.iter().map(|s| s.variant());
        let keywords = keywords.iter().map(|kw| kw.variant());
        let non_terminals = non_terminals.iter().map(|nt| nt.0.to_token_stream());

        let variants = special.chain(symbols).chain(keywords).chain(non_terminals);

        quote! {
            /// The different types of terminals and non-terminals in the
            /// OpenSCAD language grammar.
            #[allow(bad_style)]
            #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, num_derive::FromPrimitive, num_derive::ToPrimitive)]
            #[repr(u16)]
            #[non_exhaustive]
            pub enum SyntaxKind {
                #(
                    #variants,
                )*
            }
        }
    }

    fn methods(&self) -> TokenStream {
        let punctuation: Vec<_> = self.symbols.iter().map(|s| &s.ident).collect();
        let keywords: Vec<_> = self.keywords.iter().map(|kw| &kw.ident).collect();

        let symbol_lookups = self.symbols.iter().map(|Punctuation { symbol, ident }| {
            quote! {
                #symbol => Some(SyntaxKind::#ident)
            }
        });

        quote! {
            impl SyntaxKind {
                /// Is this [`SyntaxKind`] a piece of punctuation?
                ///
                /// ```rust
                /// # use scad_syntax::T;
                /// let kind = T![;];
                /// assert!(kind.is_punctuation())
                /// ```
                pub const fn is_punctuation(self) -> bool {
                    match self {
                        #(SyntaxKind::#punctuation)|* => true,
                        _ => false,
                    }
                }

                /// Is this [`SyntaxKind`] a keyword?
                ///
                /// ```rust
                /// # use scad_syntax::T;
                /// let kind = T![function];
                /// assert!(kind.is_keyword())
                /// ```
                pub const fn is_keyword(self) -> bool {
                    match self {
                        #(SyntaxKind::#keywords)|* => true,
                        _ => false,
                    }
                }

                /// Given a textual symbol try to get the associated
                /// [`SyntaxKind`].
                ///
                /// # Examples
                ///
                /// ```rust
                /// # use scad_syntax::SyntaxKind;
                /// let plus = SyntaxKind::from_symbol("+").unwrap();
                /// assert_eq!(plus, SyntaxKind::PLUS);
                /// ```
                pub fn from_symbol(symbol: &str) -> Option<Self> {
                    match symbol {
                        #(#symbol_lookups,)*
                        _ => None,
                    }
                }

                /// get the [`SyntaxKind`] that corresponds to a particular
                /// variant.
                ///
                /// ```rust
                /// # use scad_syntax::SyntaxKind;
                /// let kind = SyntaxKind::IDENT;
                /// let code: u16 = kind.into();
                ///
                /// let round_tripped = SyntaxKind::from_code(code).unwrap();
                ///
                /// assert_eq!(round_tripped, kind);
                /// ```
                pub fn from_code(n: u16) -> Option<Self> {
                    <SyntaxKind as num_traits::FromPrimitive>::from_u16(n)
                }
            }
        }
    }

    fn impls(&self) -> TokenStream {
        quote! {
            impl From<rowan::SyntaxKind> for SyntaxKind {
                fn from(k: rowan::SyntaxKind) -> Self {
                    SyntaxKind::from_code(k.0).unwrap()
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
        }
    }

    fn t_macro(&self) -> TokenStream {
        let SyntaxKind {
            keywords, symbols, ..
        } = self;

        let keywords = keywords.iter().map(|Keyword { word, ident }| {
            let token: TokenStream = word.parse().unwrap();
            (token, ident)
        });
        let symbols = symbols.iter().map(|Punctuation { symbol, ident }| {
            let symbols_needing_quotes = "()[]{}";

            let token = if symbols_needing_quotes.contains(symbol) {
                proc_macro2::Literal::string(symbol).into_token_stream()
            } else {
                symbol.parse().unwrap()
            };
            (token, ident)
        });

        let macro_arms = keywords.chain(symbols).map(|(pattern, ident)| {
            quote! {
                (#pattern) => { $crate::SyntaxKind::#ident };
            }
        });

        quote! {
            /// A helper macro for getting the [`SyntaxKind`] that corresponds
            /// to a particular token.
            #[macro_export]
            macro_rules! T {
                #( #macro_arms )*
            }
        }
    }
}

impl ToTokens for SyntaxKind {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.definition());
        tokens.extend(self.methods());
        tokens.extend(self.impls());
        tokens.extend(self.t_macro());
    }
}

#[derive(Debug, Clone)]
struct Keyword {
    word: String,
    ident: Ident,
}

impl Keyword {
    fn variant(&self) -> TokenStream {
        let Keyword { word, ident } = self;
        let docs = format!("The `{word}` keyword.");
        quote! {
            #[doc = #docs]
            #ident
        }
    }
}

#[derive(Debug, Clone)]
struct SpecialToken {
    docs: &'static str,
    ident: Ident,
}

impl SpecialToken {
    fn variant(&self) -> TokenStream {
        let SpecialToken { docs, ident } = self;
        quote! {
            #[doc = #docs]
            #ident
        }
    }
}

#[derive(Debug, Clone)]
struct Punctuation {
    symbol: String,
    ident: Ident,
}

impl Punctuation {
    fn variant(&self) -> TokenStream {
        let Punctuation { symbol, ident } = self;

        let docs = format!("The `{symbol}` symbol.");
        quote! {
            #[doc = #docs]
            #ident
        }
    }
}

fn symbol_name(symbol: &str) -> &'static str {
    PUNCTUATION_NAMES
        .iter()
        .copied()
        .find_map(|(s, n)| if s == symbol { Some(n) } else { None })
        .unwrap_or_else(|| unreachable!("No symbol for \"{symbol}\""))
}

#[derive(Debug, Clone)]
struct NonTerminal(Ident);
