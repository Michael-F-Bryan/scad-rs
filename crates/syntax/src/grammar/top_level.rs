use crate::{
    grammar::{
        expressions::{self},
        TokenSet,
    },
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
            assignment_statement(p, m);
        }
        IDENT if p.nth_at(1, T!["("]) => {
            module_instantiation(p, m);
        }
        T![function] => {
            named_function_definition(p, m);
        }
        T![module] => {
            named_module_definition(p, m);
        }
        T![if] => {
            if_statement(p, m);
        }
        _ => p.error_recover("Expected a statement", m, CONTINUE),
    }
}

fn if_statement(p: &mut Parser<'_>, m: Mark) {
    p.bump(T![if]);
    p.expect(T!["("]);
    expressions::expr(p);
    p.expect(T![")"]);
    actions(p);

    if p.eat(T![else]) {
        actions(p);
    }

    p.complete(m, IF_STATEMENT);
}

fn actions(p: &mut Parser<'_>) {
    if p.at(T!["{"]) {
        braced_actions(p);
    } else {
        action(p);
    }
}

fn braced_actions(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T!["{"]);

    while !p.eat(T!["}"]) && !p.at(EOF) {
        action(p);
    }

    p.complete(m, BRACED_ACTIONS);
}

fn action(p: &mut Parser<'_>) {
    let m = p.start();

    match p.current() {
        T![if] => if_statement(p, m),
        IDENT if p.nth_at(1, T![=]) => {
            assignment_statement(p, m);
        }
        IDENT if p.nth_at(1, T!["("]) => {
            module_instantiation(p, m);
        }
        _ => p.error_recover("Expected an action", m, CONTINUE),
    }
}

fn named_module_definition(p: &mut Parser<'_>, m: Mark) {
    p.bump(T![module]);
    p.expect(IDENT);
    p.expect(T!["("]);
    parameters(p);
    p.expect(T![")"]);
    p.expect(T!["{"]);

    while !p.at(T!["}"]) {
        statement(p);
    }
    p.expect(T!["}"]);
    p.complete(m, NAMED_MODULE_DEFINITION);
}

fn named_function_definition(p: &mut Parser<'_>, m: Mark) {
    p.bump(T![function]);
    p.expect(IDENT);
    p.expect(T!["("]);
    parameters(p);
    p.expect(T![")"]);
    p.expect(T![=]);
    expressions::expr(p);
    p.expect(T![;]);
    p.complete(m, NAMED_FUNCTION_DEFINITION);
}

fn parameters(p: &mut Parser<'_>) {
    let m = p.start();

    while p.at(IDENT) {
        parameter(p);

        if !p.eat(T![,]) {
            break;
        }
    }

    p.complete(m, PARAMETERS);
}

fn parameter(p: &mut Parser<'_>) {
    if p.nth_at(1, T![=]) {
        assignment(p);
    } else {
        let m = p.start();
        p.bump(IDENT);
        p.complete(m, IDENT);
    }
}

fn module_instantiation(p: &mut Parser<'_>, m: Mark) {
    p.bump(IDENT);
    p.bump(T!["("]);
    expressions::arguments(p);
    p.expect(T![")"]);
    child(p);
    p.complete(m, MODULE_INSTANTIATION);
}

fn children(p: &mut Parser<'_>) {
    let m = p.start();

    while matches!(p.current(), T![;] | T!["{"] | IDENT) {
        child(p);
    }

    p.complete(m, CHILDREN);
}

fn child(p: &mut Parser<'_>) {
    match p.current() {
        T![;] => {
            let m = p.start();
            p.bump(T![;]);
            p.complete(m, T![;]);
        }
        T!["{"] => {
            let m = p.start();
            p.bump(T!["{"]);
            children(p);
            p.expect(T!["}"]);
            p.complete(m, BRACED_CHILDREN);
        }
        IDENT => {
            let m = p.start();
            module_instantiation(p, m);
        }
        _ => unreachable!(),
    }
}

fn assignment_statement(p: &mut Parser<'_>, m: Mark) {
    assignment(p);
    p.expect(T![;]);
    p.complete(m, ASSIGNMENT_STATEMENT);
}

pub(crate) fn include(p: &mut Parser<'_>, m: Mark) {
    p.bump(T![include]);

    if p.eat(FILE) {
        p.complete(m, INCLUDE);
    } else {
        p.error_recover("Expected a file name", m, CONTINUE);
    }
}

pub(crate) fn use_(p: &mut Parser<'_>, m: Mark) {
    p.bump(T![use]);

    if p.eat(FILE) {
        p.complete(m, USE);
    } else {
        p.error_recover("Expected a file name", m, CONTINUE);
    }
}

pub(crate) fn assignment(p: &mut Parser<'_>) {
    let m = p.start();
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
        include_statement: statement("include <./foo/bar>"),
        use_statement: statement("use <./foo/bar>"),
        assignment_statement: statement("x = 42;"),
        kitchen_sink: package(r#"
            include <foo/bar.scad>
            use <baz.scad>
            a = 42;
            b = (42);
            c = 1 + 1;
            d = "Hello, World!";
        "#),
        cube: statement("cube([2,3,4]);"),
        multiple_module_instantiations: statement("translate([a, b, c]) cube([2, 3, 4]);"),
        module_with_children: statement("
            translate([a, b, c]) {
                cube(10);
                cube(20);
            }"
        ),
        function_definition_no_params: statement("function x() = 5;"),
        function_definition_single_unnamed_param: statement("function x(y) = y + 2;"),
        function_definition_single_param_with_default: statement("function x(y=2) = y + 2;"),
        function_definition_multiple_params: statement("function x(a, b=2, c) = a+b+c;"),
        move_module: statement("
            module move(x=0,y) {
                translate() rotate(5) children();
                cube();
            }"
        ),
        if_statement: statement("if (test) cube();"),
        if_else_statement: statement("if (test) truthy(); else falsy();"),
        if_else_statement_with_braces: statement("
            if (test) {
                truthy_1();
                truthy_2();
            } else {
                falsy();
            }"
        ),
         if_else_if_nested: statement("
            if (test1)
                if (test2) {scope2_1();}
                else {scope2_2();}
            else
                if (test3) {scope3_1();}
                else {scope3_2();}"
        )
    }
}
