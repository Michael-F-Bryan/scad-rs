use crate::{
    grammar::{assignment, top_level, TokenSet},
    parser::{Mark, Parser},
    SyntaxKind::*,
};

const LIST_COMP_TOKENS: TokenSet = TokenSet::new([T![for], T![if], T![let]]);

/// Parse an expression.
///
/// To handle precedence, we split the grammar for an expression up, using one
/// rule per precedence level.
///
/// ```ebnf
/// expression     → ternary
/// ternary        → equality ( "?" equality ":" equality )?
/// equality       → comparison ( ( "!=" | "==" ) comparison )?
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )?
/// term           → factor ( ( "-" | "+" ) factor )?
/// factor         → unary ( ( "/" | "*" ) unary )?
/// unary          → ( "!" | "-" ) unary
///                | primary
/// atom        → NUMBER
///             | STRING
///             | function_call
///             | "true"
///             | "false"
///             | "nil"
///             | "(" expression ")"
/// ```
pub(crate) fn expr(p: &mut Parser<'_>) {
    match p.current() {
        T![true] | T![false] | T![undef] | STRING | INTEGER | FLOAT | IDENT | T!["("] => {
            ternary(p);
        }
        T![-] | T![!] | T![+] => {
            unary_expr(p);
        }
        T!["["] if p.nth_at(1, T![for]) => {
            list_comprehension_expr(p);
        }
        T!["["] => {
            list_or_range_expr(p);
        }
        other => {
            p.error(format!("Expected an expression but found {other:?}"));
            p.do_bump(1);
        }
    }
}

fn eat_bin_op(p: &mut Parser<'_>, tokens: impl Into<TokenSet>) -> bool {
    let current = p.current();

    if p.at(tokens) {
        let m = p.start();
        p.bump(current);
        p.complete(m, current);
        true
    } else {
        false
    }
}

/// ```ebnf
/// ternary → equality ( "?" equality ":" equality )?
/// ```
fn ternary(p: &mut Parser<'_>) {
    let m = p.start();
    equality(p);

    if p.eat(T![?]) {
        equality(p);
        p.expect(T![:]);
        equality(p);
        p.complete(m, TERNARY_EXPR);
    } else {
        p.cancel(m);
    }
}

/// ```ebnf
/// equality → comparison ( ( "!=" | "==" ) comparison )*
/// ```
fn equality(p: &mut Parser<'_>) {
    let m = p.start();
    comparison(p);

    if !p.at(T![!=] | T![==]) {
        p.cancel(m);
        return;
    }

    while eat_bin_op(p, T![!=] | T![==]) {
        comparison(p);
    }

    p.complete(m, BIN_EXPR);
}

/// ```ebnf
/// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
/// ```
fn comparison(p: &mut Parser<'_>) {
    let m = p.start();
    term(p);

    if !p.at(T![!=] | T![==]) {
        p.cancel(m);
        return;
    }

    while eat_bin_op(p, T![!=] | T![==]) {
        term(p);
    }

    p.complete(m, BIN_EXPR);
}

/// ```ebnf
/// term → factor ( ( "-" | "+" ) factor )*
/// ```
fn term(p: &mut Parser<'_>) {
    let m = p.start();
    factor(p);

    if !p.at(T![-] | T![+]) {
        p.cancel(m);
        return;
    }

    while eat_bin_op(p, T![-] | T![+]) {
        factor(p);
    }

    p.complete(m, BIN_EXPR);
}

/// ```ebnf
/// factor → unary ( ( "/" | "*" ) unary )*
/// ```
fn factor(p: &mut Parser<'_>) {
    let m = p.start();
    unary(p);

    if !p.at(T![*] | T![/]) {
        p.cancel(m);
        return;
    }

    while eat_bin_op(p, T![*] | T![/]) {
        unary(p);
    }

    p.complete(m, BIN_EXPR);
}

/// ```ebnf
/// unary → ( "!" | "-" ) expr
///       | atom
/// ```
fn unary(p: &mut Parser<'_>) {
    let m = p.start();

    if eat_bin_op(p, T![!] | T![-]) {
        unary(p);
        p.complete(m, UNARY_EXPR);
    } else {
        p.cancel(m);
        atom(p);
    }
}

fn list_comprehension_expr(p: &mut Parser<'_>) {
    let m = p.start();

    p.bump(T!["["]);
    for_clause(p);
    p.expect(T!["]"]);

    p.complete(m, LIST_COMPREHENSION_EXPR);
}

fn list_comprehension_element(p: &mut Parser<'_>) {
    match p.current() {
        T![for] => for_clause(p),
        T![let] => let_clause(p),
        T![if] => if_clause(p),
        _ => {
            p.error("Expected a list comprehension");
        }
    }
}

fn list_comprehension_elements_or_expr(p: &mut Parser<'_>) {
    if LIST_COMP_TOKENS.contains(p.current()) {
        list_comprehension_element(p);
    } else {
        expr(p);
    }
}

pub(crate) fn assignments(p: &mut Parser<'_>) {
    let m = p.start();

    while p.at(IDENT) {
        assignment(p);

        if !p.eat(T![,]) {
            break;
        }
    }

    p.complete(m, ASSIGNMENTS);
}

fn for_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![for]);
    p.expect(T!["("]);
    assignments(p);
    p.expect(T![")"]);
    list_comprehension_elements_or_expr(p);
    p.complete(m, FOR_CLAUSE);
}

fn let_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![let]);
    p.expect(T!["("]);
    assignments(p);
    p.expect(T![")"]);
    list_comprehension_elements_or_expr(p);
    p.complete(m, LET_CLAUSE);
}

fn if_clause(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![if]);
    p.expect(T!["("]);
    expr(p);
    p.expect(T![")"]);
    list_comprehension_elements_or_expr(p);
    p.complete(m, IF_CLAUSE);
}

fn list_or_range_expr(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T!["["]);

    if p.eat(T!["]"]) {
        // it's an empty list
        p.complete(m, LIST_EXPR);
        return;
    }

    expr(p);

    if p.at(T![:]) {
        rest_of_range_expr(p, m);
    } else {
        rest_of_list_expr(p, m);
    }
}

fn rest_of_range_expr(p: &mut Parser<'_>, m: Mark) {
    p.bump(T![:]);

    expr(p);

    let has_step = p.at(T![:]);
    if has_step {
        p.bump(T![:]);
        expr(p);
    }

    p.expect(T!["]"]);

    if has_step {
        p.complete(m, RANGE_EXPR_FROM_TO_STEP);
    } else {
        p.complete(m, RANGE_EXPR_FROM_TO);
    }
}

fn rest_of_list_expr(p: &mut Parser<'_>, m: Mark) {
    while p.eat(T![,]) {
        expr(p);
    }

    p.expect(T!["]"]);

    p.complete(m, LIST_EXPR);
}

/// ```ebnf
/// atom        → NUMBER
///             | STRING
///             | function_call
///             | "true"
///             | "false"
///             | "undef"
/// ```
fn atom(p: &mut Parser<'_>) {
    match p.current() {
        IDENT => {
            if p.nth_at(1, T!["("]) {
                function_call(p);
            } else {
                lookup_expr(p);
            }
        }
        T!["("] => paren_expr(p),
        STRING | INTEGER | FLOAT | T![true] | T![false] | T![undef] => literal(p),
        other => todo!("{other:?}"),
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

    unary_op(p);

    expr(p);
    p.complete(m, UNARY_EXPR);
}

fn unary_op(p: &mut Parser<'_>) {
    let m = p.start();
    let current = p.current();
    p.bump(T![-] | T![!] | T![+]);
    p.complete(m, current);
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
        basic_list_comprehension: expr("[for(i = vector) i]"),
        #[ignore = "Not implemented in the grammar yet"]
        multiple_generator_expressions: expr("
            [
                // first expression generating the points in the positive Y quadrant
                for (a = [0, steps]) [ a, 10 * sin(a * 360 / steps) + 10 ],
                // second expression generating the points in the negative Y quadrant
                for (a = [steps, -1, 0])
                    if (a % 2 == 0)
                        let (c = a+a)
                            [ a, 10 * cos(c * 360 / steps) - 20 ],
                // additional list of fixed points
                [ 10, -3 ], [ 3, 0 ], [ 10, 3 ]
            ];",
        ),
        list_comprehension_nested_loops: expr("[ for (a = [0,2], b = [0,2]) a+b]"),
        list_comprehension_with_if_and_let: expr("
            [
                for (a = [steps, -1, 0]) if (b) let (c = d) [a, f]
            ]
        "),
        expr_add: expr("1 + 1"),
        expr_sub: expr("1 - 1"),
        expr_mul: expr("1 * 1"),
        expr_div: expr("1 / 1"),
        expr_add_and_mul_with_precedence: expr("1 + 2*2"),
        expr_mul_and_add_with_precedence: expr("1*2 + 2"),
        expr_add_and_mul_with_parens: expr("(1+2) * 2"),
        expr_add_and_subtract: expr("1 + 5 - 1"),
        precedence_kitchen_sink: expr("1/2 + 4 == 1 - -2*2"),
        unary_not: unary_expr("!true"),
        unary_negative: unary_expr("-5"),
        unary_negative_with_expr: unary_expr("-(5*2)"),
        ternary_expr: expr("condition ? truthy : falsy"),
        range_expr: expr("[a:b]"),
        range_expr_with_step: expr("[a:b:c]"),
    }
}
