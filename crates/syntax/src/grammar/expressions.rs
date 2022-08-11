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

pub(crate) fn expr(p: &mut Parser<'_>) {
    match p.current() {
        T![true] | T![false] | T![undef] | STRING | INTEGER | FLOAT | IDENT => {
            // TODO: Introduce a Pratt parser here
            let m = p.start();
            atom(p);

            if BINARY_OPERANDS.contains(p.current()) {
                binary_expr(p, m);
            } else {
                p.cancel(m);
            }
        }
        T!["("] => {
            paren_expr(p);
        }
        T![-] | T![!] | T![+] => {
            unary_expr(p);
        }
        T!["["] => {
            vector(p);
        }
        other => {
            p.error(format!("Expected an expression but found {other:?}"));
            p.do_bump(1);
        }
    }
}

fn vector(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T!["["]);

    expressions(p);

    p.expect(T!["]"]);
    p.complete(m, VECTOR_EXPR);
}

fn expressions(p: &mut Parser<'_>) {
    let m = p.start();

    expr(p);

    while p.eat(T![,]) {
        expr(p);
    }
    p.complete(m, EXPRESSIONS);
}

fn atom(p: &mut Parser<'_>) {
    match p.current() {
        IDENT => {
            if p.nth_at(1, T!["("]) {
                function_call(p);
            } else {
                lookup_expr(p);
            }
        }
        _ => {
            literal(p);
        }
    }
}

/// Wrap the next token in a node with the same kind.
fn literal(p: &mut Parser<'_>) {
    let m = p.start();
    let kind = p.current();
    p.bump(kind);
    p.complete(m, kind);
}

fn paren_expr(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T!["("]);
    expr(p);
    p.expect(T![")"]);
    p.complete(m, PAREN_EXPR);
}

fn lookup_expr(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(IDENT);

    while p.eat(T![.]) {
        p.expect(IDENT);
    }

    p.complete(m, LOOKUP_EXPR);
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

fn binary_expr(p: &mut Parser<'_>, m: Mark) {
    bin_op(p);

    expr(p);
    p.complete(m, BIN_EXPR);
}

fn bin_op(p: &mut Parser<'_>) {
    assert!(BINARY_OPERANDS.contains(p.current()));
    let m = p.start();
    let kind = p.current();
    p.bump(kind);
    p.complete(m, kind);
}

#[cfg(test)]
mod tests {
    use super::*;

    parse_tests! {
        single_number: expr("42"),
        one_plus_one: expr("1+ 1"),
        parens: expr("(42)"),
        variable_lookup: expr("x"),
        dotted_variable_lookup: expr("foo.bar.baz"),
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
