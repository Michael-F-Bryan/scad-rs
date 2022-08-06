use crate::{
    grammar::TokenSet,
    parser::{Mark, Parser},
    SyntaxKind::{*},
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

pub(crate) fn expr(p: &mut Parser<'_>) -> bool {
    let lookahead = p.nth(1);

    match p.current() {
        INTEGER | FLOAT if BINARY_OPERANDS.contains(lookahead) => {
            let m = p.start();
            p.bump(p.current());
            binary_op(p, m)
        }
        INTEGER | FLOAT => {
            p.bump(p.current());
            true
        }
        L_PAREN => {
            p.bump(T!["("]);
            expr(p);
            p.expect(T![")"])
        }
        _ => todo!(),
    }
}

fn binary_op(p: &mut Parser<'_>, m: Mark) -> bool {
    assert!(BINARY_OPERANDS.contains(p.current()));
    p.bump(p.current());

    if expr(p) {
        p.complete(m, BIN_EXPR);
        true
    } else {
        todo!();
    }
}
