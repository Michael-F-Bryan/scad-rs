use rowan::{ast::AstNode, TextRange};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    value: T,
    location: Location,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Location {
    pub text_range: TextRange,
}

impl Location {
    pub fn for_node(node: &impl AstNode) -> Self {
        let text_range = node.syntax().text_range();
        Location { text_range }
    }
}
