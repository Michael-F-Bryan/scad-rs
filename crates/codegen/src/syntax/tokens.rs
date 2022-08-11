use heck::ToShoutySnekCase;
use proc_macro2::Ident;
use quote::format_ident;
use ungrammar::{Grammar, Rule};

pub(crate) fn all_tokens(grammar: &Grammar) -> Vec<Token> {
    let mut tokens = Vec::new();
    tokens.extend(special_tokens());
    tokens.extend(punctuation());

    fn extend(grammar: &Grammar, rule: &Rule, tokens: &mut Vec<Token>) {
        match rule {
            Rule::Labeled { rule, .. } => extend(grammar, rule, tokens),
            Rule::Seq(rules) | Rule::Alt(rules) => {
                rules.iter().for_each(|r| extend(grammar, r, tokens))
            }
            Rule::Opt(rule) | Rule::Rep(rule) => extend(grammar, rule, tokens),
            Rule::Node(_) => {}
            Rule::Token(t) => {
                let symbol = &grammar[*t].name;
                let already_seen = tokens.iter().any(|t| t.token.as_deref() == Some(symbol));

                if !already_seen {
                    tokens.push(Token {
                        docs: format!("The `{symbol}` keyword."),
                        token: Some(symbol.clone()),
                        syntax_kind: format_ident!("{}_KW", symbol.TO_SHOUTY_SNEK_CASE()),
                        kind: TokenKind::Keyword,
                    });
                }
            }
        }
    }

    for node in grammar.iter() {
        let rule = &grammar[node].rule;
        extend(grammar, rule, &mut tokens);
    }

    tokens.sort_by_cached_key(|t| t.syntax_kind.clone());

    tokens
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
    ("#", "HASH"),
];

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    /// The `SyntaxKind` variant.
    pub syntax_kind: Ident,
    /// The string used by ungrammar when referring to this item.
    pub token: Option<String>,
    pub docs: String,
}

fn special_tokens() -> Vec<Token> {
    vec![
        Token {
            docs: "End of input.".to_string(),
            syntax_kind: format_ident!("EOF"),
            token: None,
            kind: TokenKind::Special,
        },
        Token {
            docs: "An identifier.".to_string(),
            syntax_kind: format_ident!("IDENT"),
            token: Some("ident".to_string()),
            kind: TokenKind::Special,
        },
        Token {
            docs: "A lexer error.".to_string(),
            syntax_kind: format_ident!("ERROR"),
            token: None,
            kind: TokenKind::Special,
        },
        Token {
            docs: "One or more whitespace characters (spaces, tabs, newlines, etc.).".to_string(),
            syntax_kind: format_ident!("WHITESPACE"),
            token: None,
            kind: TokenKind::Special,
        },
        Token {
            docs: "A comment.".to_string(),
            syntax_kind: format_ident!("COMMENT"),
            token: None,
            kind: TokenKind::Special,
        },
        Token {
            docs: "An integer literal.".to_string(),
            syntax_kind: format_ident!("INTEGER"),
            token: Some("integer".to_string()),
            kind: TokenKind::Special,
        },
        Token {
            docs: "A float literal.".to_string(),
            syntax_kind: format_ident!("FLOAT"),
            token: Some("float".to_string()),
            kind: TokenKind::Special,
        },
        Token {
            docs: "A string literal.".to_string(),
            syntax_kind: format_ident!("STRING"),
            token: Some("string".to_string()),
            kind: TokenKind::Special,
        },
        Token {
            docs: "The path to a file.".to_string(),
            syntax_kind: format_ident!("FILE"),
            token: Some("file".to_string()),
            kind: TokenKind::Special,
        },
    ]
}

fn punctuation() -> Vec<Token> {
    PUNCTUATION_NAMES
        .iter()
        .copied()
        .map(|(symbol, syntax_kind)| {
            let is_keyword = symbol.chars().all(|c| c.is_alphabetic());

            Token {
                docs: if is_keyword {
                    format!("The `{symbol}` keyword.")
                } else {
                    format!("The `{symbol}` symbol.")
                },
                token: Some(symbol.to_string()),
                syntax_kind: format_ident!("{syntax_kind}"),
                kind: TokenKind::Symbol,
            }
        })
        .collect()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Symbol,
    Keyword,
    Special,
}
