use rowan::SyntaxToken;

use crate::OpenSCAD;

impl crate::ast::Parameter {
    pub fn ident(&self) -> Option<SyntaxToken<OpenSCAD>> {
        match self {
            crate::ast::Parameter::Ident(i) => i.first_token(),
            crate::ast::Parameter::Assignment(a) => a.ident_token(),
        }
    }
}
