use crate::{
    grammar::{expressions, TokenSet},
    parser::{Mark, Parser},
    SyntaxKind::*,
};

const CONTINUE: TokenSet = TokenSet::new([T![;], T!["}"]]);

pub(crate) fn package(p: &mut Parser<'_>) {
    let m = p.start();

    while !p.at(EOF) {
        statement(p);
    }

    p.complete(m, PACKAGE);
}

pub(crate) fn statement(p: &mut Parser<'_>) {
    let m = p.start();

    match p.current() {
        T![include] => include(p, m),
        T![use] => use_(p, m),
        IDENT if p.nth_at(1, T![=]) => {
            assignment(p, m);
            p.expect(T![;]);
        }
        _ => p.error_recover("Expected a statement", m, CONTINUE),
    }
}

pub(crate) fn include(p: &mut Parser<'_>, m: Mark) {
    p.bump(T![include]);

    if p.at(STRING) {
        p.bump(STRING);
        p.complete(m, INCLUDE);
    } else {
        p.error_recover("Expected a file name", m, CONTINUE);
    }
}

pub(crate) fn use_(p: &mut Parser<'_>, m: Mark) {
    p.bump(T![use]);

    if p.at(STRING) {
        p.bump(STRING);
        p.complete(m, USE);
    } else {
        p.error_recover("Expected a file name", m, CONTINUE);
    }
}

pub(crate) fn assignment(p: &mut Parser<'_>, m: Mark) {
    p.bump(IDENT);

    p.eat(T![=]);

    expressions::expr(p);
    p.complete(m, ASSIGNMENT);
}

#[cfg(test)]
mod tests {
    use super::*;

    parse_tests! {
        empty_package: package(""),
        include_statement: statement(r#"include "./foo/bar""#),
        use_statement: statement(r#"use "./foo/bar""#),
        assignment_statement: statement("x = 42;"),
        kitchen_sink: package(r#"
            include "foo/bar.scad"
            use "baz.scad"
            a = 42;
            b = (42);
            c = 1 + 1;
        "#),
    }
}
