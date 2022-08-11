use crate::{
    grammar::{top_level, TokenSet},
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

fn _precedence(p: &Parser<'_>) -> usize {
    match p.current() {
        T![=] => 1,
        T![<] | T![<=] | T![>] | T![>=] => 2,
        T![+] | T![-] => 3,
        T![*] | T![/] | T![^] => 4,
        _ => unreachable!(),
    }
}

pub(crate) fn expr(p: &mut Parser<'_>) {
    let lookahead = p.nth(1);

    match p.current() {
        IDENT if lookahead == T!["("] => {
            function_call(p);
        }
        T![true] | T![false] | T![undef] | STRING | INTEGER | FLOAT | IDENT => {
            if BINARY_OPERANDS.contains(lookahead) {
                let m = p.start();
                p.bump(p.current());
                binary_op(p, m);
            } else {
                p.bump(p.current());
            }
        }
        T!["("] => {
            p.bump(T!["("]);
            expr(p);
            p.expect(T![")"]);
        }
        T![-] | T![!] | T![+] => {
            unary_expr(p);
        }
        other => {
            p.error(format!("Expected an expression but found {other:?}"));
            p.do_bump(1);
        }
    }
}

pub(crate) fn unary_expr(p: &mut Parser<'_>) {
    let m = p.start();

    assert!((T![-] | T![!] | T![+]).contains(p.current()));
    p.bump(p.current());

    expr(p);
    p.complete(m, UNARY_EXPR);
}

pub(crate) fn function_call(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(IDENT);
    p.bump(T!["("]);
    arguments(p);
    p.expect(T![")"]);
    p.complete(m, FUNCTION_CALL);
}

pub(crate) fn arguments(p: &mut Parser<'_>) {
    let m = p.start();

    while !p.at(T![")"]) && !p.at(EOF) {
        argument(p);

        if !p.eat(T![,]) {
            break;
        }
    }

    p.complete(m, ARGUMENTS);
}

pub(crate) fn argument(p: &mut Parser<'_>) {
    if p.at(IDENT) && p.nth_at(1, T![=]) {
        top_level::assignment(p);
    } else {
        expr(p);
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
        negative_number: expr("-42"),
        not: expr("!true"),
        positive_number: expr("+42"),
        function_call: expr("foo()"),
        function_call_with_single_arg: expr("foo(42)"),
        function_call_with_multiple_arguments: expr("foo(42, a)"),
        function_call_with_named_arguments: expr("foo(42, a = 5)"),
    }
}
