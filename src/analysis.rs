use crate::parser::{Def, Expr, ExprKind, Prog};

#[allow(dead_code)]
fn find_lambda_captures_expr(
    expr: &mut Expr,
    global_definitions: &Vec<String>,
    current_scope: &mut Vec<String>, // current scope only, not the surrounding lexical scopes
    captures: &mut Vec<String>,
) {
    macro_rules! find_lambda_captures_expr {
        ($e:expr) => {
            find_lambda_captures_expr($e, global_definitions, current_scope, captures)
        };
    }

    match &mut expr.kind {
        ExprKind::Boolean(_)
        | ExprKind::Int8(_)
        | ExprKind::Int16(_)
        | ExprKind::Int32(_)
        | ExprKind::Int64(_)
        | ExprKind::Float32(_)
        | ExprKind::Float64(_) => {}
        ExprKind::Identifier(id) => match current_scope.iter().rev().find(|&name| name == id) {
            Some(_) => {} // already exists in the **current** lexical scope
            None => {
                // check if the name exits in the global definitions, othewise it's a captured name
                if let None = global_definitions.iter().find(|&name| name == id) {
                    captures.push(id.to_owned())
                }
            }
        },
        ExprKind::Add(left, right)
        | ExprKind::Sub(left, right)
        | ExprKind::Mul(left, right)
        | ExprKind::Div(left, right)
        | ExprKind::LT(left, right)
        | ExprKind::GT(left, right)
        | ExprKind::LE(left, right)
        | ExprKind::GE(left, right)
        | ExprKind::EQ(left, right) => {
            find_lambda_captures_expr!(left);
            find_lambda_captures_expr!(right);
        }
        ExprKind::If(cond, then_, else_) => {
            find_lambda_captures_expr!(cond);
            find_lambda_captures_expr!(then_);
            find_lambda_captures_expr!(else_);
        }
        ExprKind::Cond(variants, else_) => {
            variants.iter_mut().for_each(|(cond, action)| {
                find_lambda_captures_expr!(cond);
                find_lambda_captures_expr!(action);
            });
            find_lambda_captures_expr!(else_);
        }
        ExprKind::While(cond, body) => {
            find_lambda_captures_expr!(cond);
            find_lambda_captures_expr!(body);
        }
        ExprKind::Let(bindings, body) => {
            let scope_prev_count = current_scope.len();

            // add let bindings to the current lexical scope
            for (name, _) in bindings {
                current_scope.push(name.to_owned());
            }

            find_lambda_captures_expr!(body);

            // reset the scope count to contain only global definitions
            current_scope.truncate(scope_prev_count);
        }
        ExprKind::Set(id, value) => {
            match current_scope.iter().rev().find(|&name| name == id) {
                Some(_) => {} // already exists in the **current** lexical scope
                None => captures.push(id.to_owned()),
            };
            find_lambda_captures_expr!(value);
        }
        ExprKind::Seq(first, rest) => {
            find_lambda_captures_expr!(first);
            rest.iter_mut()
                .for_each(|expr| find_lambda_captures_expr!(expr));
        }
        ExprKind::Lambda(parameters, _, body, captures) => {
            // new current scope
            let mut current_scope = vec![];

            // add lambda parameters to the current lexical scope
            for (name, _) in parameters {
                current_scope.push(name.to_owned());
            }

            // analyse the body of lambda, providing the captures buffer to fill it
            find_lambda_captures_expr(body, global_definitions, &mut current_scope, captures);
        }
        ExprKind::App(function, arguments) => {
            find_lambda_captures_expr!(function);
            arguments
                .iter_mut()
                .for_each(|expr| find_lambda_captures_expr!(expr));
        }
    }
}

#[allow(dead_code)]
pub fn find_lambda_captures(prog: &mut Prog) {
    let global_definitions = prog
        .definitions
        .iter()
        .map(|def| match def {
            Def::Fn(name, _, _, _) => name.clone(),
        })
        .collect::<Vec<String>>();

    prog.definitions
        .iter_mut()
        .for_each(|mut def| match &mut def {
            Def::Fn(_, parameters, _, body) => {
                let mut current_scope = vec![];

                // add parameters to the current lexical scope
                for (name, _) in parameters {
                    current_scope.push(name.to_owned());
                }

                // analyse the body of the function
                find_lambda_captures_expr(
                    body,
                    &global_definitions,
                    &mut current_scope,
                    &mut vec![],
                );
            }
        });

    find_lambda_captures_expr(
        &mut prog.expression,
        &global_definitions,
        &mut vec![],
        &mut vec![],
    );
}
