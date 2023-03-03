use crate::{
    parser::{Def, Expr, ExprKind, Prog},
    type_checker::{Type, TypeMap},
};

// Semantic Analysis (currently only to set lambda captures)

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
        ExprKind::Boolean(_) | ExprKind::Int8(_) | ExprKind::Int64(_) | ExprKind::Float64(_) => {}
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

    prog.definitions.iter_mut().for_each(|def| match def {
        Def::Fn(_, parameters, _, body) => {
            let mut current_scope = vec![];

            // add parameters to the current lexical scope
            for (name, _) in parameters {
                current_scope.push(name.to_owned());
            }

            // analyse the body of the function
            find_lambda_captures_expr(body, &global_definitions, &mut current_scope, &mut vec![]);
        }
    });
    find_lambda_captures_expr(
        &mut prog.expression,
        &global_definitions,
        &mut vec![],
        &mut vec![],
    );
}

// Compiler

enum Location {
    Index(u32),    // index on the stack
    FunctionLabel, // label (pointer), indenitifed by the name
}

fn generate_distinct_label(label: &str) -> String {
    static mut ID: u64 = 0;
    let id: u64;
    unsafe {
        id = ID;
        ID += 1;
    }
    format!("{}_{}", label, id)
}

fn generate_lambda_label(id: u64) -> String {
    // this won't clash with any function names, since the symbol `$` is escaped
    // in function labels
    format!("_lam_${}", id)
}

fn generate_f64_label(id: u64) -> String {
    // this won't clash with any function names, since the symbol `$` is escaped
    // in function labels
    format!("_f64_${}", id)
}

// buraq identifiers are sequence of non-terminating characters, i.e., whitespace
// characters or parentheses, these identifiers cannot be used as labels, instead
// this function escapes the necessary characters them so that they could be used
// as labels
fn sanitize_function_name(name: &String) -> String {
    let mut sanitized = String::new();

    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            sanitized.push(ch);
        } else {
            sanitized.push_str(&format!("_{0:x}_", ch as u32));
        }
    }

    format!("_fn_{}", sanitized)
}

#[inline]
fn stack_location(index: u32) -> String {
    format!("[rsp - {}]", index)
}

#[inline]
fn type_size(type_: &Type) -> u32 {
    match type_ {
        Type::I8 => 1,
        Type::I64 => 8,
        Type::F64 => 8,
        Type::Boolean => 1,
        Type::Fn(_, _) => 8,
    }
}

fn name_location(name: &String, env: &Vec<(String, Location)>) -> String {
    match env.iter().rev().find(|(id, _)| name == id) {
        Some((_, Location::Index(index))) => stack_location(*index),
        Some((name, Location::FunctionLabel)) => sanitize_function_name(name),
        None => unreachable!(),
    }
}

fn compile_expr(
    expr: &Expr,
    stack_index: u32, // bytes addressed
    env: &mut Vec<(String, Location)>,
    exprs_types: &TypeMap,
) -> String {
    macro_rules! compile_expr {
        ($e:expr) => {
            compile_expr($e, stack_index, env, exprs_types)
        };
    }

    match &expr.kind {
        // literals
        ExprKind::Boolean(true) => String::from("    mov eax, 1"),
        ExprKind::Boolean(false) => String::from("    mov eax, 0"),
        ExprKind::Int8(number) => format!("    mov eax, {}", number),
        ExprKind::Int64(number) => format!("    mov rax, {}", number),
        ExprKind::Float64(_) => format!(
            "    movsd xmm0, QWORD [{}]
    movq rax, xmm0",
            generate_f64_label(expr.id)
        ),

        // identifier
        ExprKind::Identifier(name) => format!("    mov rax, {}", name_location(name, env)),

        // arithmetics
        ExprKind::Add(left, right) => {
            let expr_type = exprs_types.get(&expr.id).unwrap();
            let left = compile_expr(left, stack_index, env, exprs_types);
            let right = compile_expr(right, stack_index + type_size(expr_type), env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left}\n    mov {left_stack_location}, rax\n{right}");

            match expr_type {
                Type::I8 => {
                    format!(
                        "{operands}
    add al, {left_stack_location}
    movsx rax, al"
                    )
                }
                Type::I64 => {
                    format!("{operands}\n    add rax, {left_stack_location}")
                }
                Type::F64 => {
                    format!(
                        "{operands}
    movq xmm0, rax
    addsd xmm0, QWORD {left_stack_location}
    movq rax, xmm0"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::Sub(left, right) => {
            let expr_type = exprs_types.get(&expr.id).unwrap();
            let left = compile_expr(left, stack_index, env, exprs_types);
            let right = compile_expr(right, stack_index + type_size(expr_type), env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left}\n    mov {left_stack_location}, rax\n{right}");

            match expr_type {
                Type::I8 => {
                    format!("{operands}
    mov ebx, eax
    mov al, BYTE {left_stack_location}
    sub al, bl
    movsx rax, al"
                    )
                }
                Type::I64 => {
                    format!(
                        "{operands}
    mov rdi, rax
    mov rax, {left_stack_location}
    sub rax, rdi"
                    )
                }
                Type::F64 => {
                    format!(
                        "{operands}
    movsd xmm0, QWORD {left_stack_location}
    movq xmm1, rax
    subsd xmm0, xmm1
    movq rax, xmm0"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::Mul(left, right) => {
            let expr_type = exprs_types.get(&expr.id).unwrap();
            let left = compile_expr(left, stack_index, env, exprs_types);
            let right = compile_expr(right, stack_index + type_size(expr_type), env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left}\n    mov {left_stack_location}, rax\n{right}");

            match expr_type {
                Type::I8 => {
                    format!(
                        "{operands}
    imul BYTE {left_stack_location}
    movsx rax, al"
                    )
                }
                Type::I64 => {
                    format!("{operands}\n    imul QWORD {left_stack_location}")
                }
                Type::F64 => {
                    format!(
                        "{operands}
    movq xmm0, rax
    mulsd xmm0, QWORD {left_stack_location}
    movq rax, xmm0"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::Div(left, right) => {
            let expr_type = exprs_types.get(&expr.id).unwrap();
            let left = compile_expr(left, stack_index, env, exprs_types);
            let right = compile_expr(right, stack_index + type_size(expr_type), env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left}\n    mov {left_stack_location}, rax\n{right}");

            match expr_type {
                Type::I8 => {
                    format!(
                        "{operands}
    mov ebx, eax
    movsx eax, BYTE {left_stack_location}
    cdq                                   ; edx = signbit(eax)
    idiv ebx
    movsx rax, al"
                    )
                }
                Type::I64 => {
                    format!(
                        "{operands}
    mov rdi, rax
    mov rax, {left_stack_location}
    cdq                                   ; edx = signbit(eax)
    idiv rdi"
                    )
                }
                Type::F64 => {
                    format!(
                        "{operands}
    movsd xmm0, QWORD {left_stack_location}
    movq xmm1, rax
    divsd xmm0, xmm1
    movq rax, xmm0"
                    )
                }
                _ => unreachable!(),
            }
        }

        // comparison
        ExprKind::LT(left, right) => {
            let left_ins = compile_expr(left, stack_index, env, exprs_types);
            let right_ins = compile_expr(right, stack_index + 1, env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left_ins}\n    mov {left_stack_location}, rax\n{right_ins}");

            match (exprs_types.get(&left.id), exprs_types.get(&right.id)) {
                (Some(Type::I64), Some(Type::I64)) => {
                    format!(
                        "{operands}
    mov rdi, {left_stack_location}
    cmp rdi, rax
    setl al
    movzx eax, al"
                    )
                }
                (Some(Type::F64), Some(Type::F64)) => {
                    format!(
                        "{operands}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    seta al
    movzx eax, al"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::GT(left, right) => {
            let left_ins = compile_expr(left, stack_index, env, exprs_types);
            let right_ins = compile_expr(right, stack_index + 1, env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left_ins}\n    mov {left_stack_location}, rax\n{right_ins}");

            match (exprs_types.get(&left.id), exprs_types.get(&right.id)) {
                (Some(Type::I64), Some(Type::I64)) => {
                    format!(
                        "{operands}
    mov rdi, {left_stack_location}
    cmp rdi, rax
    setg al
    movzx eax, al"
                    )
                }
                (Some(Type::F64), Some(Type::F64)) => {
                    format!(
                        "{operands}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    setb al
    movzx eax, al"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::LE(left, right) => {
            let left_ins = compile_expr(left, stack_index, env, exprs_types);
            let right_ins = compile_expr(right, stack_index + 1, env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left_ins}\n    mov {left_stack_location}, rax\n{right_ins}");

            match (exprs_types.get(&left.id), exprs_types.get(&right.id)) {
                (Some(Type::I64), Some(Type::I64)) => {
                    format!(
                        "{operands}
    mov rdi, {left_stack_location}
    cmp rdi, rax
    setle al
    movzx eax, al"
                    )
                }
                (Some(Type::F64), Some(Type::F64)) => {
                    format!(
                        "{operands}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    setae al
    movzx eax, al"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::GE(left, right) => {
            let left_ins = compile_expr(left, stack_index, env, exprs_types);
            let right_ins = compile_expr(right, stack_index + 1, env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left_ins}\n    mov {left_stack_location}, rax\n{right_ins}");

            match (exprs_types.get(&left.id), exprs_types.get(&right.id)) {
                (Some(Type::I64), Some(Type::I64)) => {
                    format!(
                        "{operands}
    mov rdi, {left_stack_location}
    cmp rdi, rax
    setge al
    movzx eax, al"
                    )
                }
                (Some(Type::F64), Some(Type::F64)) => {
                    format!(
                        "{operands}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    setbe al
    movzx eax, al"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::EQ(left, right) => {
            let left_ins = compile_expr(left, stack_index, env, exprs_types);
            let right_ins = compile_expr(right, stack_index + 1, env, exprs_types);
            let left_stack_location = stack_location(stack_index);
            let operands = format!("{left_ins}\n    mov {left_stack_location}, rax\n{right_ins}");

            match (exprs_types.get(&left.id), exprs_types.get(&right.id)) {
                (Some(Type::I64), Some(Type::I64)) => {
                    format!(
                        "{operands}
    mov rdi, {left_stack_location}
    cmp rdi, rax
    sete al
    movzx eax, al"
                    )
                }
                (Some(Type::F64), Some(Type::F64)) => {
                    format!(
                        "{operands}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    sete al
    movzx eax, al"
                    )
                }
                _ => unreachable!(),
            }
        }

        // constructs
        ExprKind::If(cond, then, else_) => {
            let else_label = generate_distinct_label("else");
            let end_label = generate_distinct_label("if_end");
            let cond = compile_expr!(cond);
            let then = compile_expr!(then);
            let else_ = compile_expr!(else_);
            format!(
                "{cond}
    cmp rax, 1
    jne near {else_label}
{then}
    jmp near {end_label}
{else_label}:
{else_}
{end_label}:"
            )
        }
        ExprKind::Cond(clauses, last_clause) => {
            let cond_end_label = generate_distinct_label("cond_end");
            let last_clause = compile_expr!(last_clause);
            let clauses = clauses.iter().fold(String::new(), |clauses, (test, form)| {
                let clause_end_label = generate_distinct_label("clause_end");
                let test = compile_expr!(test);
                let form = compile_expr!(form);

                format!(
                    "{clauses}
{test}
    cmp rax, 1
    jne near {clause_end_label}
{form}
    jmp near {cond_end_label}
{clause_end_label}:"
                )
            });
            format!("{clauses}\n{last_clause}\n{cond_end_label}:")
        }
        ExprKind::While(cond, body) => {
            let start_label = generate_distinct_label("while_start");
            let exit_label = generate_distinct_label("while_exit");
            let cond = compile_expr!(cond);
            let body = compile_expr!(body);
            format!(
                "{start_label}:
{cond}
    cmp rax, 1
    jne near {exit_label}
{body}
    jmp near {start_label}
{exit_label}:
"
            )
        }
        ExprKind::Let(bindings, body) => {
            let previous_bindings_count = env.len();
            let mut bindings_ins = String::new();
            let mut stack_index = stack_index;

            for (name, value) in bindings {
                let value = compile_expr(value, stack_index, env, exprs_types);

                bindings_ins.push_str(&value);
                bindings_ins.push_str(&format!("\n    mov {}, rax\n", stack_location(stack_index)));

                env.push((String::clone(name), Location::Index(stack_index)));
                stack_index += 1;
            }

            let body = compile_expr(body, stack_index, env, exprs_types);
            env.truncate(previous_bindings_count);

            format!("{bindings_ins}{body}")
        }
        ExprKind::Set(name, value) => {
            let value = compile_expr!(value);
            format!("{value}\n    mov {}, rax", name_location(name, env))
        }
        ExprKind::Seq(first, rest) => rest.iter().fold(compile_expr!(first), |output, expr| {
            format!("{output}\n{}", compile_expr!(expr))
        }),
        ExprKind::Lambda(parameters, _, body, _captures) => {
            let lambda_label = generate_lambda_label(expr.id);
            let after_lambda_label = generate_distinct_label("after_lambda");
            let previous_env_count = env.len();

            // push the function parameters stack index into the environment
            for i in 0..parameters.len() {
                let entry = (parameters[i].0.to_owned(), Location::Index(i as u32 + 1));
                env.push(entry)
            }

            // +1, because the first entry on the stack is reserved for the return address
            let stack_index = parameters.len() as u32 + 1;
            let body = compile_expr(body, stack_index, env, exprs_types);

            env.truncate(previous_env_count);
            format!(
                "    ; jump after the lambda definition
    jmp {after_lambda_label}
{lambda_label}:
{body}
    ret
{after_lambda_label}:
    ; move the lambda pointer (value) into rax
    mov rax, {lambda_label}"
            )
        }
        ExprKind::App(function, arguments) => {
            let after_call_label = generate_distinct_label("after_call");
            let function = compile_expr!(function);

            // even stack index is not 16-bytes aligned, since `rsp -= (stack_index + 1) * 8`
            let stack_needs_align = stack_index & 0x1 == 0x0;
            let arguments = arguments
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                    // +2, because the previous RSP and the return address are pushed
                    // into the stack before the called function arguments
                    let stack_index = stack_index + i as u32 + stack_needs_align as u32 + 2;
                    format!(
                        "{0}\n    mov {1}, rax",
                        compile_expr(arg, stack_index, env, exprs_types),
                        stack_location(stack_index)
                    )
                })
                .collect::<Vec<String>>()
                .join("\n");

            format!(
                "{function}
    ; save function address
    mov rcx, rax
    ; push previous RSP
    mov {0}, rsp
    mov rax, {after_call_label}
    ; save return address
    mov {1}, rax
{arguments}
    ; move RSP to the caller frame
    sub rsp, {2}
    ; call the function
    jmp rcx
{after_call_label}:
    ; pop previous RSP
    mov rsp, [rsp]",
                stack_location(stack_index + stack_needs_align as u32),
                stack_location(stack_index + stack_needs_align as u32 + 1),
                (stack_index + stack_needs_align as u32 + 1) * 8 // return address slot
            )
        }
    }
}

fn compile_defs(
    definitions: &Vec<Def>,
    env: &mut Vec<(String, Location)>,
    exprs_types: &TypeMap,
) -> String {
    fn compile_def(def: &Def, env: &mut Vec<(String, Location)>, exprs_types: &TypeMap) -> String {
        // Frame:
        //  [ previous rsp value ]
        //  [ return address     ] <- current rsp
        //                            (index++)
        //  [ argument_1 (most left)  ]
        //  | ...                     |
        //  [ argument_N (most right) ] (index += N)

        match def {
            Def::Fn(name, parameters, _, body) => {
                let previous_env_count = env.len();

                // push the function parameters stack index into the environment
                for i in 0..parameters.len() {
                    let entry = (parameters[i].0.to_owned(), Location::Index(i as u32 + 1));
                    env.push(entry)
                }

                // +1, because the first entry on the stack is reserved for the return address
                let stack_index = parameters.len() as u32 + 1;
                let body = compile_expr(body, stack_index, env, exprs_types);

                env.truncate(previous_env_count);
                format!("{}:\n{body}\n    ret", sanitize_function_name(name))
            }
        }
    }

    definitions
        .iter()
        .map(|def| compile_def(def, env, exprs_types))
        .collect::<Vec<String>>()
        .join("\n")
}

fn compile_expr_data(expr: &Expr) -> String {
    use ExprKind::*;

    match &expr.kind {
        Float64(number) => format!("{}:\n    dq {:e}\n", generate_f64_label(expr.id), number),
        Add(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        Sub(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        Mul(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        Div(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        LT(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        GT(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        LE(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        GE(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        EQ(left, right) => format!("{}{}", compile_expr_data(left), compile_expr_data(right)),
        If(cond, then, else_) => format!(
            "{}{}{}",
            compile_expr_data(cond),
            compile_expr_data(then),
            compile_expr_data(else_)
        ),
        Cond(clauses, else_) => {
            let mut clauses_data = String::new();
            for (test, form) in clauses {
                clauses_data.push_str(&format!(
                    "{}{}",
                    compile_expr_data(test),
                    compile_expr_data(form)
                ));
            }
            format!("{}{}", clauses_data, compile_expr_data(else_))
        }
        While(cond, body) => format!("{}{}", compile_expr_data(cond), compile_expr_data(body)),
        Let(values, body) => {
            let mut values_data = String::new();
            for (_, value) in values {
                values_data.push_str(&compile_expr_data(value));
            }
            format!("{}{}", values_data, compile_expr_data(body))
        }
        Set(_, value) => compile_expr_data(value),
        Seq(first, rest) => {
            let mut data = compile_expr_data(first);
            for expr in rest {
                data.push_str(&compile_expr_data(expr));
            }
            data
        }
        Lambda(_, _, body, _) => compile_expr_data(body),
        App(function, arguments) => {
            let mut data = compile_expr_data(function);
            for argument in arguments {
                data.push_str(&compile_expr_data(argument));
            }
            data
        }
        _ => String::new(),
    }
}

fn compile_data(prog: &Prog) -> String {
    let mut result = String::new();

    for def in &prog.definitions {
        match def {
            Def::Fn(_, _, _, body) => result.push_str(&compile_expr_data(&body)),
        }
    }

    result.push_str(&compile_expr_data(&prog.expression));
    return result;
}

pub fn compile(prog: &Prog, exprs_types: &TypeMap) -> String {
    // construct the initial environment from the global definitions
    let mut env: Vec<(String, Location)> = prog
        .definitions
        .iter()
        .map(|def| match def {
            Def::Fn(name, _, _, _) => (name.to_owned(), Location::FunctionLabel),
        })
        .collect();

    format!(
        "    section .data
{}
    section .text
    global boot
{}
boot:
    sub rsp, 8
{}
    add rsp, 8
    ret",
        compile_data(&prog),
        compile_defs(&prog.definitions, &mut env, exprs_types),
        compile_expr(&prog.expression, 1, &mut env, exprs_types)
    )
}
