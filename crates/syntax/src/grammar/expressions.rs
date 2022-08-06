use crate::{
    grammar::TokenSet,
    parser::{Mark, Parser},
    SyntaxKind::*,
};

const BINARY_OPERANDS: TokenSet = TokenSet::new([
    T![+],
    T![-],
    T![*],
    T![/],
    T![%],
    T![>=],
    T![>],
    T![=],
    T![<=],
    T![<],
    T![&&],
    T![||],
]);

pub(crate) fn expr(p: &mut Parser<'_>) {
    match p.current() {
        T![true] | T![false] | T![undef] | STRING | INTEGER | FLOAT | IDENT => {
            let lookahead = p.nth(1);
            if BINARY_OPERANDS.contains(lookahead) {
                let m = p.start();
                p.bump(p.current());
                binary_op(p, m);
            } else {
                p.bump(p.current());
            }
        }
        L_PAREN => {
            p.bump(T!["("]);
            expr(p);
            p.expect(T![")"]);
        }
        other => {
            p.error(format!("Expected an expression but found {other:?}"));
            p.do_bump(1);
        }
    }
}

fn binary_op(p: &mut Parser<'_>, m: Mark) {
    assert!(BINARY_OPERANDS.contains(p.current()));
    p.bump(p.current());

    expr(p);
    p.complete(m, BIN_EXPR);
}

#[cfg(test)]
mod tests {
    use super::*;

    parse_tests! {
        single_number: expr("42"),
        one_plus_one: expr("1+ 1"),
        parens: expr("(42)"),
        variable_lookup: expr("x"),
        true_expr: expr("true"),
        false_expr: expr("false"),
        undef_expr: expr("undef"),
        string_expr: expr(r#""Hello, World!""#),
        #[ignore = "Negative numbers aren't implemented yet"]
        negative_number: expr("-42"),
    }
}
