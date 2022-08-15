use crate::SyntaxToken;

impl crate::ast::Parameter {
    pub fn ident(&self) -> Option<SyntaxToken> {
        match self {
            crate::ast::Parameter::Ident(i) => i.first_token(),
            crate::ast::Parameter::Assignment(a) => a.ident_token(),
        }
    }
}
