macro_rules! ast_node_union {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $(
                $variant:ident($ty:ty)
            ),*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis enum $name {
            $($variant($ty)),*
        }

        impl rowan::ast::AstNode for $name {
            type Language = scad_syntax::OpenSCAD;

            fn can_cast(kind: scad_syntax::SyntaxKind) -> bool
            where
                Self: Sized,
            {
                $( <$ty>::can_cast(kind) )||*
            }

            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
            where
                Self: Sized,
            {
                $(
                    if let Some(node) = <$ty>::cast(node.clone()) {
                        return Some($name::$variant(node));
                    }
                )*
                None
            }

            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
                match self {
                    $(
                        $name::$variant(n) => n.syntax(),
                    )*
                }
            }
        }

        $(
            impl From<$ty> for $name {
                fn from(node: $ty) -> Self {
                    $name::$variant(node)
                }
            }

            impl TryFrom<$name> for $ty {
                type Error = $crate::macros::ConversionFailed;

                fn try_from(value: $name) -> Result<$ty, Self::Error> {
                    match value {
                        $name::$variant(v) => Ok(v),
                        _ => Err($crate::macros::ConversionFailed),
                    }
                }
            }
        )*
    };
}

#[derive(Debug, Default)]
pub struct ConversionFailed;