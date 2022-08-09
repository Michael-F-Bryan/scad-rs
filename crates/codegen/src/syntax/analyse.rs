use std::collections::BTreeSet;

use anyhow::{Context, Error};
use heck::{ToPascalCase, ToShoutySnekCase};
use indexmap::IndexMap;
use proc_macro2::Ident;
use quote::format_ident;
use ungrammar::{Grammar, Rule};

#[derive(Debug, Clone)]
pub(crate) struct SyntacticElements {
    pub(crate) keywords: Vec<Keyword>,
    pub(crate) symbols: Vec<Punctuation>,
    pub(crate) special: Vec<SpecialToken>,
    pub(crate) enums: Vec<EnumNode>,
    pub(crate) structs: Vec<StructNode>,
    pub(crate) tokens: Vec<TokenNode>,
}

impl SyntacticElements {
    pub(crate) fn from_grammar(grammar: &Grammar) -> Result<Self, Error> {
        let tokens: BTreeSet<_> = grammar
            .iter()
            .flat_map(|id| {
                let node = &grammar[id];
                all_tokens(&node.rule, grammar)
            })
            .collect();

        let (keywords, symbols): (Vec<_>, Vec<_>) = tokens
            .into_iter()
            .partition(|name| name.chars().all(|c| c.is_alphabetic()));

        let special = special_tokens();

        let keywords: Vec<_> = keywords
            .into_iter()
            .filter(|kw| {
                special
                    .iter()
                    .filter_map(|s| s.rule_name)
                    .all(|name| kw != name)
            })
            .map(|word| Keyword {
                ident: format_ident!("{}_KW", word.TO_SHOUTY_SNEK_CASE()),
                word,
            })
            .collect();
        let symbols: Vec<_> = symbols
            .into_iter()
            .map(|s: String| Punctuation {
                ident: format_ident!(
                    "{}",
                    symbol_name(&s).unwrap_or_else(|| unreachable!("No symbol for \"{s}\""))
                ),
                symbol: s,
            })
            .collect();

        let mut enums = Vec::new();
        let mut structs = Vec::new();
        let mut tokens = Vec::new();

        for id in grammar.iter() {
            let node = &grammar[id];
            let name = &node.name;
            let _span = tracing::debug_span!("process_rule", %name).entered();

            match process_rule(name, &node.rule, grammar)
                .with_context(|| format!("Unable to process \"{}\"", node.name))?
            {
                AstNodeKind::Struct(s) => structs.push(s),
                AstNodeKind::Enum(e) => enums.push(e),
                AstNodeKind::Token(t) => tokens.push(t),
            }
        }

        Ok(SyntacticElements {
            enums,
            keywords,
            special,
            structs,
            symbols,
            tokens,
        })
    }
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
                Rule::Node(n) => Ok(AstNodeKind::Struct(StructNode {
                    ident,
                    syntax_kind,
                    fields: vec![Field {
                        rule_name: grammar[*n].name.clone(),
                        label: None,
                        multiplicity: Multiplicity::Multiple,
                        is_token: false,
                    }],
                })),
                Rule::Token(t) => Ok(AstNodeKind::Struct(StructNode {
                    ident,
                    syntax_kind,
                    fields: vec![Field {
                        rule_name: grammar[*t].name.clone(),
                        label: None,
                        multiplicity: Multiplicity::Multiple,
                        is_token: true,
                    }],
                })),
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
    let mut fields: IndexMap<String, Field> = IndexMap::new();

    for (i, rule) in rules.iter().enumerate() {
        tracing::debug!(index = i, ?rule, "Processing field");

        for field in struct_field(rule, grammar)? {
            match fields.entry(field.rule_name.clone()) {
                indexmap::map::Entry::Occupied(mut occupied) => {
                    (*occupied.get_mut()).multiplicity = Multiplicity::Multiple;
                }
                indexmap::map::Entry::Vacant(vacant) => {
                    vacant.insert(field);
                }
            }
        }
    }

    Ok(fields.into_values().collect())
}

fn struct_field(rule: &Rule, grammar: &Grammar) -> Result<Vec<Field>, Error> {
    match rule {
        Rule::Token(t) => {
            let name = &grammar[*t].name;
            Ok(vec![Field {
                label: None,
                multiplicity: Multiplicity::One,
                rule_name: symbol_name(name).unwrap_or(name).to_pascal_case(),
                is_token: true,
            }])
        }
        Rule::Node(n) => {
            let rule_name = grammar[*n].name.clone();
            Ok(vec![Field {
                rule_name,
                label: None,
                multiplicity: Multiplicity::One,
                is_token: false,
            }])
        }
        Rule::Labeled { label, rule } => {
            let mut fields = struct_field(rule, grammar)?;

            if let Some(field) = fields.pop() {
                Ok(vec![Field {
                    label: Some(label.clone()),
                    ..field
                }])
            } else {
                Ok(Vec::new())
            }
        }
        Rule::Opt(rule) => {
            let mut fields = struct_field(rule, grammar)?;
            anyhow::ensure!(fields.len() == 1);
            Ok(vec![Field {
                multiplicity: Multiplicity::Optional,
                ..fields.remove(0)
            }])
        }
        Rule::Rep(rule) => {
            let mut fields = struct_field(rule, grammar)?;
            Ok(vec![Field {
                multiplicity: Multiplicity::Multiple,
                ..fields.remove(0)
            }])
        }
        Rule::Seq(seq) => {
            let mut fields = Vec::new();

            for rule in seq {
                fields.extend(struct_field(rule, grammar)?);
            }

            Ok(fields)
        }
        Rule::Alt(variants) if variants.iter().all(|v| matches!(v, Rule::Token(_))) => {
            // It's fine to have something like op:("+" | "-")
            Ok(vec![])
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
        Rule::Token(t) => {
            let name = &grammar[*t].name;

            Ok(Some(Variant {
                label: None,
                multiplicity: Multiplicity::One,
                rule_name: symbol_name(name).unwrap_or(name).to_pascal_case(),
                is_token: true,
            }))
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
            is_token: false,
        })),
        Rule::Seq(seq) if seq.iter().filter(|r| matches!(r, Rule::Node(_))).count() == 1 => {
            match seq[0] {
                Rule::Node(n) => Ok(Some(Variant {
                    rule_name: grammar[n].name.clone(),
                    label: None,
                    multiplicity: Multiplicity::One,
                    is_token: false,
                })),
                Rule::Token(t) => Ok(Some(Variant {
                    rule_name: grammar[t].name.clone(),
                    label: None,
                    multiplicity: Multiplicity::One,
                    is_token: true,
                })),
                _ => unreachable!(),
            }
        }
        Rule::Opt(_) => {
            anyhow::bail!("It isn't valid for a rule in an alternate to be optional");
        }
        _ => anyhow::bail!("Expected a single token or node"),
    }
}

fn symbol_name(symbol: &str) -> Option<&'static str> {
    PUNCTUATION_NAMES
        .iter()
        .copied()
        .find_map(|(s, n)| if s == symbol { Some(n) } else { None })
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

fn special_tokens() -> Vec<SpecialToken> {
    vec![
        SpecialToken {
            docs: "End of input.",
            ident: format_ident!("EOF"),
            rule_name: None,
        },
        SpecialToken {
            docs: "An identifier.",
            ident: format_ident!("IDENT"),
            rule_name: Some("ident"),
        },
        SpecialToken {
            docs: "A lexer error.",
            ident: format_ident!("ERROR"),
            rule_name: None,
        },
        SpecialToken {
            docs: "One or more whitespace characters (spaces, tabs, newlines, etc.).",
            ident: format_ident!("WHITESPACE"),
            rule_name: None,
        },
        SpecialToken {
            docs: "A comment.",
            ident: format_ident!("COMMENT"),
            rule_name: None,
        },
        SpecialToken {
            docs: "An integer literal",
            ident: format_ident!("INTEGER"),
            rule_name: Some("integer"),
        },
        SpecialToken {
            docs: "A float literal",
            ident: format_ident!("FLOAT"),
            rule_name: Some("float"),
        },
        SpecialToken {
            docs: "A string literal",
            ident: format_ident!("STRING"),
            rule_name: Some("string"),
        },
    ]
}

#[derive(Debug, Clone)]
pub(crate) struct SpecialToken {
    pub(crate) docs: &'static str,
    /// The string used by ungrammar when referring to a token.
    pub(crate) rule_name: Option<&'static str>,
    pub(crate) ident: Ident,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumNode {
    pub(crate) ident: Ident,
    pub(crate) variants: Vec<Variant>,
}

#[derive(Debug, Clone)]
pub(crate) struct Variant {
    pub(crate) rule_name: String,
    pub(crate) label: Option<String>,
    pub(crate) multiplicity: Multiplicity,
    pub(crate) is_token: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct StructNode {
    pub(crate) ident: Ident,
    pub(crate) syntax_kind: Ident,
    pub(crate) fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub(crate) struct Keyword {
    pub(crate) word: String,
    pub(crate) ident: Ident,
}

#[derive(Debug, Clone)]
pub(crate) struct Punctuation {
    pub(crate) symbol: String,
    pub(crate) ident: Ident,
}

#[derive(Debug, Clone)]
pub(crate) struct Field {
    pub(crate) rule_name: String,
    pub(crate) label: Option<String>,
    pub(crate) multiplicity: Multiplicity,
    pub(crate) is_token: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Multiplicity {
    Optional,
    One,
    Multiple,
}

#[derive(Debug, Clone)]
enum AstNodeKind {
    Struct(StructNode),
    Enum(EnumNode),
    Token(TokenNode),
}

#[derive(Debug, Clone)]
pub(crate) struct TokenNode {
    pub(crate) symbol: String,
    pub(crate) ident: Ident,
}
