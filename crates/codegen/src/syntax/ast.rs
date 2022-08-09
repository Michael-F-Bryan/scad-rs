use heck::{ToPascalCase, ToShoutySnekCase, ToSnekCase};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::syntax::{EnumNode, Field, Multiplicity, StructNode, SyntacticElements, Variant};

#[derive(Debug, Clone)]
pub struct Ast {
    structs: Vec<StructNode>,
    enums: Vec<EnumNode>,
}

impl Ast {
    pub(crate) fn new(elements: &SyntacticElements) -> Self {
        let SyntacticElements { enums, structs, .. } = elements;

        Ast {
            enums: enums.clone(),
            structs: structs.clone(),
        }
    }
}

impl ToTokens for Ast {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Ast { enums, structs } = self;

        enums
            .iter()
            .map(|e| e as &dyn ToTokens)
            .chain(structs.iter().map(|s| s as &dyn ToTokens))
            .for_each(|tok| tok.to_tokens(tokens));
    }
}

impl EnumNode {
    fn definition(&self) -> TokenStream {
        let EnumNode { ident, variants } = self;
        let variants = variants.iter().map(|v| {
            let type_name = v.type_name();
            let name = v.name();

            if v.is_token {
                quote!(#name(rowan::api::SyntaxNode<crate::OpenSCAD>))
            } else {
                quote!(#name(#type_name))
            }
        });

        quote! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub enum #ident {
                #(#variants,)*
            }
        }
    }

    fn methods(&self) -> TokenStream {
        let EnumNode { ident, variants } = self;

        let getter = variants.iter().map(|v| {
            let type_name = if v.is_token {
                quote!(rowan::api::SyntaxNode<crate::OpenSCAD>)
            } else {
                v.type_name().to_token_stream()
            };
            let name = v.name();
            let method_name = format_ident!("as_{}", name.to_string().to_snek_case());

            quote! {
                pub fn #method_name(&self) -> Option<#type_name> {
                    match self {
                        #ident :: #name(node) => Some(node.clone()),
                        _ => None,
                    }
                }
            }
        });

        quote! {
            impl #ident {
                #( #getter )*
            }
        }
    }

    fn conversions(&self) -> TokenStream {
        let EnumNode { ident, variants } = self;
        let mut tokens = TokenStream::new();

        for variant in variants {
            if variant.multiplicity != Multiplicity::One || variant.is_token {
                continue;
            }

            let type_name = variant.type_name();
            let name: Ident = variant.name();

            let t = quote! {
                impl From<#type_name> for #ident {
                    fn from(value: #type_name) -> #ident {
                        #ident::#name(value)
                    }
                }
            };
            tokens.extend(t);
        }

        tokens
    }

    fn ast_node_impl(&self) -> TokenStream {
        let EnumNode { ident, variants } = self;

        let can_casts = variants.iter().map(|v| {
            let type_name = v.type_name();

            if v.is_token {
                let kind = v.syntax_kind();
                quote!(kind == #kind)
            } else {
                quote!(<#type_name as rowan::ast::AstNode>::can_cast(kind))
            }
        });

        let try_casts = variants.iter().map(|v| {
            let type_name = v.type_name();

            if !v.is_token {
                quote! {
                    if let Some(node) = <#type_name as rowan::ast::AstNode>::cast(node.clone()) {
                        return Some(node.into());
                    }
                }
            } else {
                let kind = v.syntax_kind();
                quote! {
                    if node.kind() == #kind {
                        return Some(Self::#type_name(node));
                    }
                }
            }
        });

        let names = variants.iter().map(|v| v.name());

        quote! {
            impl rowan::ast::AstNode for #ident {
                type Language = crate::OpenSCAD;

                fn can_cast(kind: crate::SyntaxKind) -> bool {
                    #( #can_casts )||*
                }

                fn cast(node: rowan::api::SyntaxNode<crate::OpenSCAD>) -> Option<Self> {
                    #( #try_casts )*

                    None
                }

                fn syntax(&self) -> &rowan::api::SyntaxNode<crate::OpenSCAD> {
                    match self {
                        #(
                            #ident::#names(node) => node.syntax(),
                        )*
                    }
                }
            }
        }
    }
}

impl ToTokens for EnumNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.definition());
        tokens.extend(self.methods());
        tokens.extend(self.ast_node_impl());
        tokens.extend(self.conversions());
    }
}

impl StructNode {
    fn definition(&self) -> TokenStream {
        let name = &self.ident;
        quote! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #name(rowan::api::SyntaxNode<crate::OpenSCAD>);
        }
    }

    fn methods(&self) -> TokenStream {
        let ident = &self.ident;

        let getters = self.fields.iter().map(|f| {
            let Field {
                rule_name,
                label,
                multiplicity,
                is_token,
            } = f;

            let mut method_name = label
                .as_deref()
                .map(|s| s.to_snek_case())
                .unwrap_or_else(|| rule_name.to_snek_case());
            if ["use", "let", "for", "if", "else", "true", "false"].contains(&method_name.as_str()) {
                method_name.insert_str(0, "r#");
            }
            let method_name = format_ident!("{method_name}");
            let type_name = format_ident!("{}", rule_name.to_pascal_case());

            if *is_token {
                let token_kind = format_ident!("{}", rule_name.TO_SHOUTY_SNEK_CASE());
                match multiplicity {
                    Multiplicity::Optional => {
                        let method_name = format_ident!("{method_name}_opt");
                        quote! {
                            pub fn #method_name(&self) -> Option<rowan::api::SyntaxToken<crate::OpenSCAD>> {
                                rowan::ast::support::token(&self.0, crate::SyntaxKind::#token_kind)
                            }
                        }
                    }
                    Multiplicity::One => quote! {
                        pub fn #method_name(&self) -> Option<rowan::api::SyntaxToken<crate::OpenSCAD>> {
                                rowan::ast::support::token(&self.0, crate::SyntaxKind::#token_kind)
                        }
                    },
                    Multiplicity::Multiple => {
                        let method_name = format_ident!("{method_name}s");
                        quote! {
                            pub fn #method_name(&self) -> impl Iterator<Item = rowan::api::SyntaxToken<crate::OpenSCAD>> {
                                self.0.children_with_tokens()
                                    .filter_map(|it| it.into_token())
                                    .filter(|tok| tok.kind() == crate::SyntaxKind::#token_kind)
                            }
                        }
                    }
                }
            } else {
                match multiplicity {
                    Multiplicity::Optional => {
                        let method_name = format_ident!("{method_name}_opt");
                        quote! {
                            pub fn #method_name(&self) -> Option<#type_name> {
                                rowan::ast::support::child(&self.0)
                            }
                        }
                    }
                    Multiplicity::One => quote! {
                        pub fn #method_name(&self) -> Option<#type_name> {
                                rowan::ast::support::child(&self.0)
                        }
                    },
                    Multiplicity::Multiple => {
                        let method_name = format_ident!("{method_name}s");
                        quote! {
                            pub fn #method_name(&self) -> impl Iterator<Item = #type_name> {
                                rowan::ast::support::children(&self.0)
                            }
                        }
                    }
                }
            }
        });

        quote! {
            impl #ident {
                #( #getters )*
            }
        }
    }

    fn ast_node_impl(&self) -> TokenStream {
        let StructNode {
            ident, syntax_kind, ..
        } = self;

        quote! {
            impl rowan::ast::AstNode for #ident {
                type Language = crate::OpenSCAD;

                fn can_cast(kind: crate::SyntaxKind) -> bool {
                    kind == crate::SyntaxKind::#syntax_kind
                }

                fn cast(node: rowan::api::SyntaxNode<crate::OpenSCAD>) -> Option<Self> {
                    if #ident::can_cast(node.kind()) {
                        Some(#ident(node))
                    } else {
                        None
                    }
                }

                fn syntax(&self) -> &rowan::api::SyntaxNode<crate::OpenSCAD> {
                    &self.0
                }
            }
        }
    }
}

impl ToTokens for StructNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.definition());
        tokens.extend(self.methods());
        tokens.extend(self.ast_node_impl());
    }
}

impl Variant {
    fn type_name(&self) -> Ident {
        format_ident!("{}", self.rule_name.to_pascal_case())
    }

    fn syntax_kind(&self) -> TokenStream {
        let variant = format_ident!("{}", self.rule_name.TO_SHOUTY_SNEK_CASE());
        quote!(crate::SyntaxKind::#variant)
    }

    fn name(&self) -> Ident {
        let name = self.label.as_deref().unwrap_or(self.rule_name.as_str());

        format_ident!("{}", name.to_pascal_case())
    }
}
