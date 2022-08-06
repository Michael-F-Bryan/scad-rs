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
    let lookahead = p.nth(1);

    match p.current() {
        T![true] | T![false] | T![undef] | STRING | INTEGER | FLOAT | IDENT
            if BINARY_OPERANDS.contains(lookahead) =>
        {
            let m = p.start();
            p.bump(p.current());
            binary_op(p, m);
        }
        T![true] | T![false] | T![undef] | STRING | INTEGER | FLOAT | IDENT
        => {
            p.bump(p.current());
        }
        L_PAREN => {
            p.bump(T!["("]);
            expr(p);
            p.expect(T![")"]);
        }
        _ => todo!(),
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
        single_number => expr("42"),
        one_plus_one => expr("1 + 1"),
        parens => expr("(42)"),
        variable_lookup => expr("x"),
    }
}
