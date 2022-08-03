use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use anyhow::{Context as _, Error};
use heck::ToShoutySnekCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use ungrammar::{Grammar, Rule};

const PUNCTUATION_NAMES: &[(&str, &str)] = &[
    (";", "SEMICOLON"),
    (":", "COLON"),
    (",", "COMMA"),
    (".", "DOT"),
    ("?", "QUESTION_MARK"),
    ("(", "L_PAREN"),
    ("{", "L_CURLY"),
    ("[", "L_BRACE"),
    (")", "R_PAREN"),
    ("}", "R_CURLY"),
    ("]", "R_BRACE"),
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

#[derive(Debug)]
pub struct Syntax {
    nodes: BTreeMap<String, AstNode>,
    syntax_kind: SyntaxKind,
}

impl Syntax {
    pub fn from_grammar(grammar: &Grammar) -> Result<Self, Error> {
        let mut nodes = BTreeMap::new();
        let mut tokens = BTreeSet::new();

        for id in grammar.iter() {
            let node = &grammar[id];
            let name = &node.name;
            let _span = tracing::debug_span!("process_rule", %name).entered();

            tokens.extend(all_tokens(&node.rule, grammar));

            let ast_node = process_rule(&node.rule, grammar)
                .with_context(|| format!("Unable to process \"{}\"", node.name))?;
            nodes.insert(name.clone(), ast_node);
        }

        let (keywords, symbols): (Vec<_>, Vec<_>) = tokens
            .into_iter()
            .partition(|name| name.chars().all(|c| c.is_alphabetic()));

        let syntax_kind = SyntaxKind {
            keywords: keywords.into_iter().map(Keyword).collect(),
            symbols: symbols
                .into_iter()
                .map(|s| Punctuation {
                    name: symbol_name(&s).to_string(),
                    symbol: s,
                })
                .collect(),
        };

        Ok(Syntax { nodes, syntax_kind })
    }

    pub fn internal_rules(&self) -> impl Iterator<Item = &'_ str> + '_ {
        self.nodes.keys().map(|s| s.as_str())
    }

    pub fn kind(&self) -> TokenStream {
        let kind = syntax_kind_definition(self);
        let methods = syntax_kind_methods(self);

        quote! {
            #kind
            #methods
        }
    }

    pub fn punctuation(&self) -> impl Iterator<Item = &'_ str> + '_ {
        self.syntax_kind.symbols.iter().map(|s| s.symbol.as_str())
    }

    pub fn keywords(&self) -> impl Iterator<Item = &'_ str> + '_ {
        self.syntax_kind.keywords.iter().map(|k| k.0.as_str())
    }
}

fn syntax_kind_methods(syntax: &Syntax) -> TokenStream {
    let punctuation: Vec<_> = syntax
        .punctuation()
        .map(symbol_name)
        .map(|name| format_ident!("{name}"))
        .collect();
    let keywords: Vec<_> = syntax
        .punctuation()
        .map(symbol_name)
        .map(|name| format_ident!("{name}"))
        .collect();

    let symbol_lookups = PUNCTUATION_NAMES.iter().map(|(symbol, name)| {
        let name = format_ident!("{}", name.TO_SHOUTY_SNEK_CASE());
        quote!(#symbol => Some(SyntaxKind::#name))
    });

    quote! {
        impl SyntaxKind {
            pub const fn is_punctuation(self) -> bool {
                match self {
                    #(SyntaxKind::#punctuation)|* => true,
                    _ => false,
                }
            }

            pub const fn is_keyword(self) -> bool {
                match self {
                    #(SyntaxKind::#keywords)|* => true,
                    _ => false,
                }
            }

            pub const fn from_symbol(symbol: &str) -> Option<SyntaxKind> {
                match symbol {
                    #(#symbol_lookups,)*
                    _ => None,
                }
            }
        }
    }
}

fn syntax_kind_definition(syntax: &Syntax) -> TokenStream {
    let special_variants = ["IDENT", "ERROR", "WHITESPACE", "COMMENT"]
        .iter()
        .map(|name| format_ident!("{name}").to_token_stream());
    let literals = ["INTEGER_LIT", "FLOAT_LIT"]
        .iter()
        .map(|name| format_ident!("{name}").to_token_stream());

    let punctuation = syntax.punctuation().map(|symbol| {
        let name = symbol_name(symbol).TO_SHOUTY_SNEK_CASE();
        let docs = format!("The `{symbol}` symbol.");
        let ident = format_ident!("{name}");

        quote! {
            #[doc = #docs]
            #ident
        }
    });

    let variants = special_variants
        .into_iter()
        .chain(literals)
        .chain(punctuation);

    quote! {
        pub enum SyntaxKind {
            #(
                #variants,
            )*
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

fn all_tokens(rule: &Rule, grammar: &Grammar) -> HashSet<String> {
    match rule {
        Rule::Rep(rule) | Rule::Opt(rule) | Rule::Labeled { rule, .. } => {
            all_tokens(&*rule, grammar)
        }
        Rule::Node(_) => HashSet::new(),
        Rule::Token(t) => [grammar[*t].name.clone()].into_iter().collect(),
        Rule::Alt(items) | Rule::Seq(items) => items
            .iter()
            .flat_map(|rule| all_tokens(rule, grammar))
            .collect(),
    }
}

fn process_rule(rule: &Rule, grammar: &Grammar) -> Result<AstNode, Error> {
    match rule {
        Rule::Seq(rules) => {
            let fields = get_struct_fields(rules, grammar)?;
            Ok(AstNode::Struct { fields })
        }
        Rule::Opt(rule) => {
            let rules = std::slice::from_ref(&**rule);
            let fields = get_struct_fields(rules, grammar)?;
            Ok(AstNode::Struct { fields })
        }
        Rule::Rep(rep) => {
            // A single repeated item
            match &**rep {
                Rule::Node(n) => {
                    let rule_name = grammar[*n].name.clone();
                    Ok(AstNode::Struct {
                        fields: vec![Field {
                            rule_name,
                            label: None,
                            multiplicity: Multiplicity::Multiple,
                        }],
                    })
                }
                _ => todo!(),
            }
        }
        Rule::Alt(a) => {
            let variants = get_enum_variants(a, grammar)?;
            Ok(AstNode::Enum { variants })
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
        Rule::Node(node) => {
            let node = &grammar[*node];
            Ok(Some(Variant {
                rule_name: node.name.clone(),
                multiplicity: Multiplicity::One,
                label: None,
            }))
        }
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
        Rule::Opt(rule) => {
            let variant = enum_variant(rule, grammar)?.map(|v| Variant {
                multiplicity: Multiplicity::Multiple,
                ..v
            });
            Ok(variant)
        }
        _ => anyhow::bail!("Expected a single token or node"),
    }
}

#[derive(Debug, Clone)]
enum AstNode {
    Enum { variants: Vec<Variant> },
    Struct { fields: Vec<Field> },
}

#[derive(Debug, Clone)]
struct Variant {
    rule_name: String,
    label: Option<String>,
    multiplicity: Multiplicity,
}

#[derive(Debug, Clone)]
struct Field {
    rule_name: String,
    label: Option<String>,
    multiplicity: Multiplicity,
}

#[derive(Debug, Clone)]
enum Multiplicity {
    Optional,
    One,
    Multiple,
}

#[derive(Debug, Clone)]
struct SyntaxKind {
    keywords: Vec<Keyword>,
    symbols: Vec<Punctuation>,
}

#[derive(Debug, Clone)]
struct Keyword(String);

#[derive(Debug, Clone)]
struct Punctuation {
    symbol: String,
    name: String,
}
