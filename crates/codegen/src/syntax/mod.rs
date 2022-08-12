mod ast_nodes;
mod syntax_kind;
mod tokens;

pub use self::{ast_nodes::ast_nodes, syntax_kind::SyntaxKind};

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use ungrammar::{Grammar, Rule};

    use super::*;

    #[test]
    fn generated_syntax_kind() {
        let project_root = crate::project_root();
        let output_folder = project_root.join("crates").join("syntax").join("src");

        let grammar = include_str!("../../../../scad.ungram")
            .replace("\r\n", "\n")
            .parse()
            .unwrap();

        let syntax_kind = SyntaxKind::new(&grammar);
        let syntax_kind_rs = output_folder.join("syntax_kind.rs");
        crate::ensure_file_contents(&syntax_kind_rs, &syntax_kind, module_path!());

        let ast_nodes = ast_nodes(&grammar, &syntax_kind);
        let ast_nodes_rs = output_folder.join("ast.rs");
        crate::ensure_file_contents(&ast_nodes_rs, &ast_nodes, module_path!());
    }

    #[test]
    fn all_rules_are_used() {
        let grammar: Grammar = include_str!("../../../../scad.ungram")
            .replace("\r\n", "\n")
            .parse()
            .unwrap();
        let mut rules: HashSet<_> = grammar.iter().map(|n| grammar[n].name.as_str()).collect();

        fn mark(rule: &Rule, rules: &mut HashSet<&str>, grammar: &Grammar) {
            match rule {
                Rule::Labeled { rule, .. } => mark(rule, rules, grammar),
                Rule::Node(n) => {
                    rules.remove(grammar[*n].name.as_str());
                }
                Rule::Token(_) => {}
                Rule::Seq(r) | Rule::Alt(r) => {
                    for rule in r {
                        mark(rule, rules, grammar);
                    }
                }
                Rule::Opt(rule) | Rule::Rep(rule) => mark(rule, rules, grammar),
            }
        }

        for n in grammar.iter() {
            mark(&grammar[n].rule, &mut rules, &grammar);
        }

        // the root node is always used
        rules.remove("Package");

        if !rules.is_empty() {
            panic!("Unused rules: {rules:?}");
        }
    }
}
