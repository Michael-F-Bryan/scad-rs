use anyhow::{Context as _, Error};
use heck::{ToPascalCase, ToShoutySnekCase, ToSnekCase};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use ungrammar::{Grammar, Rule};

#[derive(Debug, Clone)]
pub struct Ast {
    structs: Vec<StructNode>,
    enums: Vec<EnumNode>,
}

impl Ast {
    pub fn from_grammar(grammar: &Grammar) -> Result<Self, Error> {
        let mut structs = Vec::new();
        let mut enums = Vec::new();

        for id in grammar.iter() {
            let node = &grammar[id];
            let name = &node.name;
            let _span = tracing::debug_span!("process_rule", %name).entered();

            match process_rule(name, &node.rule, grammar)
                .with_context(|| format!("Unable to process \"{}\"", node.name))?
            {
                AstNodeKind::Struct(s) => structs.push(s),
                AstNodeKind::Enum(e) => enums.push(e),
            }
        }

        Ok(Ast { enums, structs })
    }

    /// The `SyntaxKind` variants used by all non-terminal rules nodes in the
    /// OpenSCAD language grammar.
    pub fn non_terminals(&self) -> impl Iterator<Item = Ident> + '_ {
        self.structs.iter().map(|s| &s.syntax_kind).cloned()
    }
}

impl ToTokens for Ast {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Ast { structs, enums } = self;
        for s in structs {
            s.to_tokens(tokens);
        }
        for e in enums {
            e.to_tokens(tokens);
        }
    }
}

#[derive(Debug, Clone)]
struct EnumNode {
    ident: Ident,
    variants: Vec<Variant>,
}

impl EnumNode {
    fn definition(&self) -> TokenStream {
        let EnumNode { ident, variants } = self;
        let variants = variants.iter().map(|v| {
            let type_name = v.type_name();
            let name = v.name();
            quote!(#name(#type_name))
        });

        quote! {
            pub enum #ident {
                #(#variants,)*
            }
        }
    }

    fn methods(&self) -> TokenStream {
        let ident = &self.ident;

        quote! {
            impl #ident {

            }
        }
    }

    fn conversions(&self) -> TokenStream {
        let EnumNode { ident, variants } = self;
        let mut tokens = TokenStream::new();

        for variant in variants {
            if variant.multiplicity != Multiplicity::One {
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
            quote!(<#type_name as rowan::ast::AstNode>::can_cast(kind))
        });

        let try_casts = variants.iter().map(|v| {
            let type_name = v.type_name();

            quote! {
                if let Some(node) = <#type_name as rowan::ast::AstNode>::cast(node.clone()) {
                    return Some(node.into());
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

#[derive(Debug, Clone)]
struct StructNode {
    ident: Ident,
    syntax_kind: Ident,
    fields: Vec<Field>,
}

impl StructNode {
    fn definition(&self) -> TokenStream {
        let name = &self.ident;
        quote! {
            pub struct #name(rowan::api::SyntaxNode<crate::OpenSCAD>);
        }
    }

    fn methods(&self) -> TokenStream {
        let ident = &self.ident;

        // FIXME: This doesn't work for rules with multiple fields of the same
        // type (e.g. `condition:Expr "?" truthy:Expr ":" falsy:Expr`)
        let getters = self.fields.iter().map(|f| {
            let Field {
                rule_name,
                label,
                multiplicity,
            } = f;

            let method_name = label
                .as_deref()
                .map(|s| s.to_snek_case())
                .unwrap_or_else(|| rule_name.to_snek_case());
            let method_name = format_ident!("{method_name}");
            let type_name = format_ident!("{}", rule_name.to_pascal_case());

            match multiplicity {
                Multiplicity::Optional => {
                    let method_name = format_ident!("{method_name}_opt");
                    quote! {
                        pub fn #method_name(&self) -> Option<#type_name> {
                            self.0.children().find_map(<#type_name as rowan::ast::AstNode>::cast)
                        }
                    }
                }
                Multiplicity::One => quote! {
                    pub fn #method_name(&self) -> Option<#type_name> {
                        self.0.children().find_map(<#type_name as rowan::ast::AstNode>::cast)
                    }
                },
                Multiplicity::Multiple => {
                    let method_name = format_ident!("{method_name}s");
                    quote! {
                        pub fn #method_name(&self) -> impl Iterator<Item = #type_name> {
                            self.0.children().filter_map(<#type_name as rowan::ast::AstNode>::cast)
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

#[derive(Debug, Clone)]
struct Variant {
    rule_name: String,
    label: Option<String>,
    multiplicity: Multiplicity,
}

impl Variant {
    fn type_name(&self) -> Ident {
        format_ident!("{}", self.rule_name.to_pascal_case())
    }

    fn name(&self) -> Ident {
        let name = self.label.as_deref().unwrap_or(self.rule_name.as_str());

        format_ident!("{}", name.to_pascal_case())
    }
}

#[derive(Debug, Clone)]
struct Field {
    rule_name: String,
    label: Option<String>,
    multiplicity: Multiplicity,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Multiplicity {
    Optional,
    One,
    Multiple,
}

enum AstNodeKind {
    Struct(StructNode),
    Enum(EnumNode),
}

fn process_rule(rule_name: &str, rule: &Rule, grammar: &Grammar) -> Result<AstNodeKind, Error> {
    let ident = format_ident!("{}", rule_name.to_pascal_case());
    let syntax_kind = format_ident!("{}", rule_name.TO_SHOUTY_SNEK_CASE());

    match rule {
        Rule::Seq(rules) => {
            let fields = get_struct_fields(rules, grammar)?;
            Ok(AstNodeKind::Struct(StructNode {
                ident,
                syntax_kind,
                fields,
            }))
        }
        Rule::Opt(rule) => {
            let rules = std::slice::from_ref(&**rule);
            let fields = get_struct_fields(rules, grammar)?;
            Ok(AstNodeKind::Struct(StructNode {
                ident,
                syntax_kind,
                fields,
            }))
        }
        Rule::Rep(rep) => {
            // A single repeated item
            match &**rep {
                Rule::Node(n) => {
                    let rule_name = grammar[*n].name.clone();
                    let fields = vec![Field {
                        rule_name,
                        label: None,
                        multiplicity: Multiplicity::Multiple,
                    }];
                    Ok(AstNodeKind::Struct(StructNode {
                        ident,
                        syntax_kind,
                        fields,
                    }))
                }
                _ => todo!(),
            }
        }
        Rule::Alt(a) => {
            let variants = get_enum_variants(a, grammar)?;
            Ok(AstNodeKind::Enum(EnumNode { ident, variants }))
        }
        _ => {
            anyhow::bail!("Expected either a set of alternatives or a sequence of tokens/rules")
        }
    }
}

fn get_struct_fields(rules: &[Rule], grammar: &Grammar) -> Result<Vec<Field>, Error> {
    let mut fields = Vec::new();

    for (i, rule) in rules.iter().enumerate() {
        tracing::debug!(index = i, ?rule, "Processing field");
        if let Some(field) = struct_field(rule, grammar)? {
            fields.push(field);
        }
    }

    fields.dedup_by(|a, b| {
        if a.rule_name == b.rule_name && a.label == b.label {
            b.multiplicity = Multiplicity::Multiple;
            true
        } else {
            false
        }
    });

    Ok(fields)
}

fn struct_field(rule: &Rule, grammar: &Grammar) -> Result<Option<Field>, Error> {
    match rule {
        Rule::Token(_) => Ok(None),
        Rule::Node(n) => {
            let rule_name = grammar[*n].name.clone();
            Ok(Some(Field {
                rule_name,
                label: None,
                multiplicity: Multiplicity::One,
            }))
        }
        Rule::Labeled { label, rule } => match struct_field(rule, grammar)? {
            Some(f) => Ok(Some(Field {
                label: Some(label.clone()),
                ..f
            })),
            None => Ok(None),
        },
        Rule::Opt(rule) => match struct_field(rule, grammar)? {
            Some(f) => Ok(Some(Field {
                multiplicity: Multiplicity::Optional,
                ..f
            })),
            None => Ok(None),
        },
        Rule::Rep(rule) => match struct_field(rule, grammar)? {
            Some(f) => Ok(Some(Field {
                multiplicity: Multiplicity::Multiple,
                ..f
            })),
            None => Ok(None),
        },
        Rule::Seq(seq) => {
            let mut fields = seq
                .iter()
                .filter_map(|r| struct_field(r, grammar).transpose())
                .collect::<Result<Vec<Field>, Error>>()?;

            match fields.len() {
                0 => todo!(),
                1 => Ok(Some(fields.remove(0))),
                _ => todo!(),
            }
        }
        Rule::Alt(variants) if variants.iter().all(|v| matches!(v, Rule::Token(_))) => {
            // It's fine to have something like op:("+" | "-")
            Ok(None)
        }
        other => todo!("Handle {other:?}"),
    }
}

fn get_enum_variants(alternatives: &[Rule], grammar: &Grammar) -> Result<Vec<Variant>, Error> {
    let mut variants = Vec::new();

    for (i, alt) in alternatives.iter().enumerate() {
        if let Some(variant) =
            enum_variant(alt, grammar).with_context(|| format!("Alternative {i}"))?
        {
            variants.push(variant);
        }
    }

    Ok(variants)
}

fn enum_variant(alt: &Rule, grammar: &Grammar) -> Result<Option<Variant>, Error> {
    match alt {
        Rule::Token(_) => {
            // we don't care about tokens when pattern matching on enums.
            Ok(None)
        }
        Rule::Labeled { label, rule } => {
            let variant = enum_variant(rule, grammar)?.map(|v| Variant {
                label: Some(label.clone()),
                ..v
            });
            Ok(variant)
        }
        Rule::Node(node) => Ok(Some(Variant {
            rule_name: grammar[*node].name.clone(),
            multiplicity: Multiplicity::One,
            label: None,
        })),
        Rule::Seq(seq) if seq.iter().filter(|r| matches!(r, Rule::Node(_))).count() == 1 => {
            let node = match seq[0] {
                Rule::Node(n) => &grammar[n],
                _ => unreachable!(),
            };
            Ok(Some(Variant {
                rule_name: node.name.clone(),
                label: None,
                multiplicity: Multiplicity::One,
            }))
        }
        Rule::Opt(_) => {
            anyhow::bail!("It isn't valid for a rule in an alternate to be optional");
        }
        _ => anyhow::bail!("Expected a single token or node"),
    }
}
