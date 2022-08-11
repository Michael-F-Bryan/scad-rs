use heck::ToShoutySnekCase;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use ungrammar::{Grammar, Rule};

use crate::syntax::tokens::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct SyntaxKind {
    pub tokens: Vec<Token>,
    pub variants: Vec<SyntaxKindVariant>,
}

impl SyntaxKind {
    pub fn new(grammar: &Grammar) -> Self {
        let tokens = super::tokens::all_tokens(grammar);
        let variants = syntax_kind_variants(grammar, &tokens);
        SyntaxKind { tokens, variants }
    }

    pub(crate) fn get_token(&self, symbol: &str) -> Option<&Token> {
        self.tokens
            .iter()
            .find(|t| t.token.as_deref() == Some(symbol))
    }

    fn definition(&self) -> TokenStream {
        let variants =
            self.variants
                .iter()
                .enumerate()
                .map(|(i, SyntaxKindVariant { docs, ident, .. })| {
                    let i = u16::try_from(i).unwrap();
                    match docs {
                        Some(docs) => quote!(#[doc = #docs] #ident = #i),
                        None => quote!(#ident = #i),
                    }
                });

        quote! {
            /// The different types of terminals and non-terminals in the
            /// OpenSCAD language grammar.
            #[derive(
                Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
            )]
            #[repr(u16)]
            #[allow(nonstandard_style)]
            #[non_exhaustive]
            pub enum SyntaxKind {
                #( #variants, )*
            }
        }
    }

    fn methods(&self) -> TokenStream {
        let SyntaxKind { tokens, variants } = self;

        let variants: Vec<_> = variants.iter().map(|v| &v.ident).collect();
        let num_variants = variants.len();

        let codes = variants.iter().enumerate().map(|(i, v)| {
            let i = u16::try_from(i).unwrap();
            quote!(#i => Some(SyntaxKind::#v))
        });

        let punctuation: Vec<_> = tokens
            .iter()
            .filter(|t| match t.kind {
                TokenKind::Symbol => true,
                _ => false,
            })
            .map(|t| &t.syntax_kind)
            .collect();

        let keywords: Vec<_> = tokens
            .iter()
            .filter(|t| match t.kind {
                TokenKind::Keyword => true,
                _ => false,
            })
            .map(|t| &t.syntax_kind)
            .collect();

        let symbol_lookups = tokens
            .iter()
            .filter_map(|t| match (t.kind, t.token.as_ref()) {
                (TokenKind::Symbol, Some(symbol)) => {
                    let ident = &t.syntax_kind;
                    Some(quote!(#symbol => Some(SyntaxKind::#ident)))
                }
                _ => None,
            });

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
                    match n {
                        #( #codes, )*
                        _ => None,
                    }
                }
            }
        }
    }

    fn t_macro(&self) -> TokenStream {
        let arms = self.tokens.iter().filter_map(
            |Token {
                 token,
                 syntax_kind,
                 kind,
                 ..
             }| {
                let token = token.clone()?;
                if matches!(kind, TokenKind::Special) {
                    return None;
                }

                let token = if "()[]{}".contains(&token) {
                    token.to_token_stream()
                } else {
                    token.parse().unwrap()
                };

                Some(quote! {
                    (#token) => { $crate::SyntaxKind::#syntax_kind }
                })
            },
        );

        quote! {
            /// A helper macro for getting the [`SyntaxKind`] that corresponds
            /// to a particular token.
            #[macro_export]
            macro_rules! T {
                #( #arms; )*
            }
        }
    }

    fn conversions(&self) -> TokenStream {
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
}

impl ToTokens for SyntaxKind {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(self.definition());
        tokens.extend(self.methods());
        tokens.extend(self.conversions());
        tokens.extend(self.t_macro());
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxKindVariant {
    pub docs: Option<String>,
    pub rule_name: Option<String>,
    pub ident: Ident,
}

fn syntax_kind_variants(grammar: &Grammar, tokens: &[Token]) -> Vec<SyntaxKindVariant> {
    let mut variants: Vec<_> = tokens
        .iter()
        .cloned()
        .map(
            |Token {
                 docs,
                 syntax_kind,
                 token,
                 ..
             }| SyntaxKindVariant {
                docs: Some(docs),
                rule_name: token,
                ident: syntax_kind,
            },
        )
        .collect();

    for node in grammar.iter() {
        let node = &grammar[node];

        if matches!(node.rule, Rule::Alt(_)) {
            continue;
        }

        let rule_name = &node.name;
        let ident = format_ident!("{}", rule_name.TO_SHOUTY_SNEK_CASE());
        variants.push(SyntaxKindVariant {
            docs: None,
            rule_name: Some(rule_name.clone()),
            ident,
        });
    }

    variants
}
