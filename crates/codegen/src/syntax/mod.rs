mod ast;
mod syntax_kind;

pub use self::{ast::Ast, syntax_kind::SyntaxKind};

use std::collections::BTreeSet;

use anyhow::Error;
use ungrammar::{Grammar, Rule};

#[derive(Debug)]
pub struct Syntax {
    pub ast: Ast,
    pub syntax_kind: SyntaxKind,
}

impl Syntax {
    pub fn from_grammar(grammar: &Grammar) -> Result<Self, Error> {
        let tokens: BTreeSet<_> = grammar
            .iter()
            .flat_map(|id| {
                let node = &grammar[id];

                all_tokens(&node.rule, grammar)
            })
            .collect();
        let ast = Ast::from_grammar(&grammar)?;

        let syntax_kind = SyntaxKind::from_tokens(tokens, ast.non_terminals());

        Ok(Syntax { ast, syntax_kind })
    }
}

fn all_tokens(rule: &Rule, grammar: &Grammar) -> BTreeSet<String> {
    match rule {
        Rule::Rep(rule) | Rule::Opt(rule) | Rule::Labeled { rule, .. } => {
            all_tokens(&*rule, grammar)
        }
        Rule::Node(_) => BTreeSet::new(),
        Rule::Token(t) => [grammar[*t].name.clone()].into_iter().collect(),
        Rule::Alt(items) | Rule::Seq(items) => items
            .iter()
            .flat_map(|rule| all_tokens(rule, grammar))
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use crate::add_preamble;

    use super::*;

    #[test]
    fn generated_syntax_code() {
        let project_root = crate::project_root();
        let output_folder = project_root.join("crates").join("syntax").join("src");
        let syntax_kind_rs = output_folder.join("syntax_kind.rs");
        let ast_rs = output_folder.join("ast.rs");
        let grammar = include_str!("./scad.ungram").parse().unwrap();

        let Syntax { ast, syntax_kind } = Syntax::from_grammar(&grammar).unwrap();

        let syntax_kind = crate::pretty_print(&syntax_kind).unwrap();
        let syntax_kind = add_preamble(env!("CARGO_PKG_NAME"), syntax_kind);
        crate::ensure_file_contents(&syntax_kind_rs, &syntax_kind);

        let ast = crate::pretty_print(&ast).unwrap();
        let ast = add_preamble(env!("CARGO_PKG_NAME"), ast);
        crate::ensure_file_contents(&ast_rs, &ast);
    }
}
