use std::collections::BTreeSet;

use heck::ToShoutySnekCase;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use ungrammar::{Grammar, Rule};

use crate::syntax::{Keyword, Punctuation, SpecialToken, SyntacticElements};

#[derive(Debug, Clone)]
pub struct SyntaxKind {
    keywords: Vec<Keyword>,
    symbols: Vec<Punctuation>,
    special: Vec<SpecialToken>,
    non_terminals: Vec<NonTerminal>,
}

impl SyntaxKind {
    pub(crate) fn new(elements: &SyntacticElements) -> Self {
        let SyntacticElements {
            keywords,
            symbols,
            special,
            structs,
            ..
        } = elements;
        SyntaxKind {
            keywords: keywords.clone(),
            symbols: symbols.clone(),
            special: special.clone(),
            non_terminals: structs
                .iter()
                .map(|s| NonTerminal(s.ident.clone()))
                .collect(),
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
        let non_terminals = non_terminals.iter().map(|nt| {
            format_ident!("{}", nt.0.to_string().TO_SHOUTY_SNEK_CASE()).to_token_stream()
        });

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

        let variants: Vec<_> = self
            .special
            .iter()
            .map(|s| &s.ident)
            .chain(self.symbols.iter().map(|p| &p.ident))
            .chain(self.keywords.iter().map(|kw| &kw.ident))
            .chain(self.non_terminals.iter().map(|nt| &nt.0))
            .collect();
        let num_variants = variants.len();

        quote! {
            impl SyntaxKind {
                /// All the possible [`SyntaxKind`] variants.
                pub const VARIANTS: [SyntaxKind; #num_variants] = [
                    #(SyntaxKind::#variants),*
                ];

                /// Is this [`SyntaxKind`] a piece of punctuation?
                ///
                /// ```rust
                /// # use scad_syntax::T;
                /// let kind = T![;];
                /// assert!(kind.is_punctuation())
                /// ```
                pub const fn is_punctuation(self) -> bool {
                    matches!(self, #(SyntaxKind::#punctuation)|*)
                }

                /// Is this [`SyntaxKind`] a keyword?
                ///
                /// ```rust
                /// # use scad_syntax::T;
                /// let kind = T![function];
                /// assert!(kind.is_keyword())
                /// ```
                pub const fn is_keyword(self) -> bool {
                    matches!(self, #(SyntaxKind::#keywords)|*)
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

fn all_tokens(rule: &Rule, grammar: &Grammar) -> BTreeSet<String> {
    match rule {
        Rule::Rep(rule) | Rule::Opt(rule) | Rule::Labeled { rule, .. } => all_tokens(rule, grammar),
        Rule::Node(_) => BTreeSet::new(),
        Rule::Token(t) => [grammar[*t].name.clone()].into_iter().collect(),
        Rule::Alt(items) | Rule::Seq(items) => items
            .iter()
            .flat_map(|rule| all_tokens(rule, grammar))
            .collect(),
    }
}

impl SpecialToken {
    fn variant(&self) -> TokenStream {
        let SpecialToken { docs, ident, .. } = self;
        quote! {
            #[doc = #docs]
            #ident
        }
    }
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

#[derive(Debug, Clone)]
struct NonTerminal(Ident);
