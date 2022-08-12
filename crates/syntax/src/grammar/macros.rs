#[cfg(test)]
macro_rules! parse_tests {
    ( $( $(#[$meta:meta])* $name:ident : $func:ident ($src:expr $(,)?) ),* $(,)?) => {
        $(
            #[test]
            $(#[$meta])*
            fn $name() {
                let mut parser = Parser::new(crate::tokenize($src));
                let m = parser.start();
                $func(&mut parser);

                // Note: to get around the whole "you can only have one
                // top-level node" thing, we use a dummy node that immediately
                // gets peeled away
                parser.complete(m, $crate::SyntaxKind::EOF);
                let (node, errors) = parser.finish();

                eprintln!("---- NODE ----");
                eprintln!("{node:#?}");
                eprintln!("---- ERRORS ----");
                eprintln!("{errors:#?}");
                assert!(errors.is_empty());

                let children: Vec<_> = node.children_with_tokens().collect();

                insta::with_settings! {
                    {
                        description => format!("{}({})", stringify!($func), $src),
                        omit_expression => true,
                    },
                    {
                        match children.as_slice() {
                            [single] => insta::assert_debug_snapshot!(single),
                            _ => insta::assert_debug_snapshot!(children),
                        }
                    }
                }
            }
        )*
    };
}
