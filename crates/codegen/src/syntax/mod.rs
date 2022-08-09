mod analyse;
mod ast;
mod syntax_kind;

pub(crate) use self::{
    analyse::{
        EnumNode, Field, Keyword, Multiplicity, Punctuation, SpecialToken, StructNode,
        SyntacticElements, Variant,
    },
    ast::Ast,
    syntax_kind::SyntaxKind,
};

use anyhow::Error;
use ungrammar::Grammar;

#[derive(Debug)]
pub struct Syntax {
    pub ast: Ast,
    pub syntax_kind: SyntaxKind,
}

impl Syntax {
    pub fn from_grammar(grammar: &Grammar) -> Result<Self, Error> {
        let elements = SyntacticElements::from_grammar(grammar)?;
        let ast = Ast::new(&elements);
        let syntax_kind = SyntaxKind::new(&elements);

        Ok(Syntax { ast, syntax_kind })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_syntax_code() {
        let project_root = crate::project_root();
        let output_folder = project_root.join("crates").join("syntax").join("src");
        let syntax_kind_rs = output_folder.join("syntax_kind.rs");
        let ast_rs = output_folder.join("ast.rs");
        let grammar = include_str!("../../../../scad.ungram")
            .replace("\r\n", "\n")
            .parse()
            .unwrap();

        let Syntax { ast, syntax_kind } = Syntax::from_grammar(&grammar).unwrap();

        let generator = env!("CARGO_PKG_NAME");
        crate::ensure_file_contents(&syntax_kind_rs, &syntax_kind, generator);
        crate::ensure_file_contents(&ast_rs, &ast, generator);
    }
}
