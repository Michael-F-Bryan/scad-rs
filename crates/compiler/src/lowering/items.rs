use im::{OrdMap, Vector};
use rowan::ast::AstNode;
use scad_syntax::{ast, SyntaxKind, SyntaxNode};

use crate::{diagnostics::DuplicateSymbol, hir, lowering::Lowering, Diagnostics, Location, Text};

pub(crate) fn declaration(db: &dyn Lowering, ident: SyntaxNode) -> Option<hir::NameDefinitionSite> {
    debug_assert_eq!(ident.kind(), SyntaxKind::IDENT);

    let names = db.named_items_in_scope(ident.clone());
    let token = ident.first_token()?;
    let name = token.text();

    names
        .iter()
        .find_map(|(n, def)| if *n == name { Some(def.clone()) } else { None })
}

pub(crate) fn top_level_items(db: &dyn Lowering) -> (OrdMap<Text, hir::Item>, Diagnostics) {
    let (pkg, mut diags) = db.ast();
    let mut items: OrdMap<Text, hir::Item> = OrdMap::new();

    for stmt in pkg.statements() {
        if let Some((name, item)) = item_name(stmt) {
            let location = Location::for_node(&item);

            if let Some(original) = items.insert(name.clone(), item.clone()) {
                diags.push(DuplicateSymbol {
                    name,
                    original_definition: Location::for_node(&original),
                    duplicate_definition: location,
                });
            }
        }
    }

    (items, diags)
}

fn item_name(stmt: ast::Statement) -> Option<(Text, hir::Item)> {
    let item = match stmt {
        ast::Statement::AssignmentStatement(a) => hir::Item::Constant(a),
        ast::Statement::NamedFunctionDefinition(f) => hir::Item::Function(f),
        ast::Statement::NamedModuleDefinition(m) => hir::Item::Module(m),
        ast::Statement::ModuleInstantiation(_) | ast::Statement::IfStatement(_) => return None,
        ast::Statement::Include(_) | ast::Statement::Use(_) => {
            todo!("Add imported functions/modules")
        }
    };

    let name = item.name()?;
    Some((name, item))
}

pub(crate) fn named_items_in_scope(
    db: &dyn Lowering,
    target: SyntaxNode,
) -> Vector<(Text, hir::NameDefinitionSite)> {
    let parent = match target.parent() {
        Some(p) => p,
        None => return Vector::new(),
    };

    let mut named_items: Vector<(Text, hir::NameDefinitionSite)> = Vector::new();

    // First, get any items that were defined before this one
    for child in parent.children() {
        let reached_target = child == target;
        if let Some(item) = hir::NameDefinitionSite::cast(child) && let Some(name) = item.name() {
            named_items.push_front((name, item));
        }

        if reached_target {
            break;
        }
    }

    // next, make sure any parameters defined by the parent are added
    if let Some(function_like) = FunctionLike::cast(parent.clone()) {
        for param in function_like.parameters() {
            if let Some(name) = param.ident() {
                named_items.push_back((Text::new(name.text()), param.into()));
            }
        }
    }

    // Recursively add any nodes defined in the parent scope
    named_items.append(db.named_items_in_scope(parent));

    named_items
}

ast_node_union! {
    #[derive(Debug)]
    enum FunctionLike {
        Function(ast::NamedFunctionDefinition),
        Module(ast::NamedModuleDefinition),
    }
}

impl FunctionLike {
    fn parameters(&self) -> impl Iterator<Item = ast::Parameter> {
        let parameters = match self {
            FunctionLike::Function(f) => f.parameters_opt(),
            FunctionLike::Module(m) => m.parameters_opt(),
        };
        parameters.into_iter().flat_map(|p| p.parameters())
    }
}

#[cfg(test)]
mod tests {
    use crate::{db::Database, parsing::Parsing};

    use super::*;

    #[test]
    fn named_items_in_scope() {
        let mut db = Database::default();
        db.set_src(Text::new(
            "
            a = 1;
            b = 2;

            function c(d, e) = f;
        ",
        ));
        let (items, _) = db.top_level_items();

        let c = items["c"].syntax();
        let names = db.named_items_in_scope(c.clone());
        // when `c` is defined, the only variables in scope should be itself,
        // `a` and `b`.
        assert_eq!(names.len(), 3);
        assert_eq!(names[0].0, "c");
        assert_eq!(names[1].0, "b");
        assert_eq!(names[2].0, "a");

        let c = ast::NamedFunctionDefinition::cast(c.clone()).unwrap();
        let f = c.expr().unwrap();

        let names = db.named_items_in_scope(f.syntax().clone());
        // We should see `d` and `e`.
        assert_eq!(names.len(), 5);
        assert_eq!(names[0].0, "d");
        assert_eq!(names[1].0, "e");
        // plus everything from before
        assert_eq!(names[2].0, "c");
        assert_eq!(names[3].0, "b");
        assert_eq!(names[4].0, "a");
    }
}
