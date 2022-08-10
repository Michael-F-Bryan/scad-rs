mod syntax_kind;
mod tokens;

pub use self::syntax_kind::SyntaxKind;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_syntax_kind() {
        let project_root = crate::project_root();
        let output_folder = project_root.join("crates").join("syntax").join("src");
        let syntax_kind_rs = output_folder.join("syntax_kind.rs");

        let grammar = include_str!("../../../../scad.ungram")
            .replace("\r\n", "\n")
            .parse()
            .unwrap();

        let syntax_kind = SyntaxKind::new(&grammar);

        let generator = env!("CARGO_PKG_NAME");
        crate::ensure_file_contents(&syntax_kind_rs, &syntax_kind, generator);
    }
}
