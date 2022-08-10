mod ast_nodes;
mod syntax_kind;
mod tokens;

pub use self::{ast_nodes::ast_nodes, syntax_kind::SyntaxKind};

#[cfg(test)]
mod tests {
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
}
