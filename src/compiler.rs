use crate::{
    parser::{Def, Expr, ExprKind, Prog},
    type_checker::{Type, TypeMap},
};

enum Location {
    Index(i64),    // index on the stack
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

fn label_lambda(id: u64) -> String {
    // this won't clash with any function names, since the symbol `$` is escaped
    // in function labels
    format!("_lam_${}", id)
}

fn label_float(id: u64) -> String {
    // this won't clash with any function names, since the symbol `$` is escaped
    // in function labels
    format!("_float_${}", id)
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
fn stack_location(index: i64) -> String {
    if index < 0 {
        format!("[rsp + {}]", -index)
    } else {
        format!("[rsp - {}]", index)
    }
}

// return the size in bytes of the given type
#[inline]
fn type_size(type_: &Type) -> i64 {
    match type_ {
        Type::I8 => 1,
        Type::I16 => 2,
        Type::I32 => 4,
        Type::I64 => 8,
        Type::F32 => 4,
        Type::F64 => 8,
        Type::Boolean => 1,
        Type::Fn(_, _) => 8, // functions are pointers
    }
}

#[inline]
fn register_with_size(size_bytes: i64) -> &'static str {
    match size_bytes {
        1 => "al",
        2 => "ax",
        4 => "eax",
        8 => "rax",
        _ => unreachable!(),
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
    stack_index: i64,    // bytes addressed
    tail_position: bool, // the last expression to be evalutated in the current frame
    env: &mut Vec<(String, Location)>,
    exprs_types: &TypeMap,
) -> String {
    macro_rules! compile_expr {
        ($e:expr, $tp:expr) => {
            compile_expr($e, stack_index, $tp, env, exprs_types)
        };
    }

    match &expr.kind {
        // literals
        ExprKind::Boolean(true) => String::from("    mov eax, 1"),
        ExprKind::Boolean(false) => String::from("    mov eax, 0"),
        ExprKind::Int8(number) => format!("    mov eax, {}", number),
        ExprKind::Int16(number) => format!("    mov eax, {}", number),
        ExprKind::Int32(number) => format!("    mov eax, {}", number),
        ExprKind::Int64(number) => format!("    mov rax, {}", number),
        ExprKind::Float32(_) => format!(
            "    movss xmm0, DWORD [{}]
    movd eax, xmm0",
            label_float(expr.id)
        ),
        ExprKind::Float64(_) => format!(
            "    movsd xmm0, QWORD [{}]
    movq rax, xmm0",
            label_float(expr.id)
        ),

        // identifier
        ExprKind::Identifier(name) => {
            let expr_type = exprs_types.get(&expr.id).unwrap();

            match type_size(expr_type) {
                1 => format!("    movzx rax, BYTE {}", name_location(name, env)),
                2 => format!("    movzx rax, WORD {}", name_location(name, env)),
                8 => format!("    mov rax, {}", name_location(name, env)),
                _ => unreachable!(),
            }
        }

        // arithmetics
        ExprKind::Add(left, right) => {
            let expr_type = exprs_types.get(&expr.id).unwrap();
            let expr_size = type_size(expr_type);
            let left_stack_index = stack_index + expr_size;

            let left = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right = compile_expr(right, left_stack_index + expr_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match expr_type {
                Type::I8 => {
                    format!(
                        "{left}
    mov {left_stack_location}, al
{right}
    add al, {left_stack_location}
    movsx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left}
    mov {left_stack_location}, ax
{right}
    add ax, {left_stack_location}
    movsx rax, ax"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left}
    mov {left_stack_location}, eax
{right}
    add eax, {left_stack_location}
    cdqe                                  ; rax = sign_extend(eax)"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left}
    mov {left_stack_location}, rax
{right}
    add rax, {left_stack_location}"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left}
    mov {left_stack_location}, eax
{right}
    movd xmm0, eax
    addss xmm0, DWORD {left_stack_location}
    movd eax, xmm0"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left}
    mov {left_stack_location}, rax
{right}
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
            let expr_size = type_size(expr_type);
            let left_stack_index = stack_index + expr_size;

            let left = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right = compile_expr(right, left_stack_index + expr_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match expr_type {
                Type::I8 => {
                    format!(
                        "{left}
    mov {left_stack_location}, al
{right}
    mov ebx, eax
    mov al, BYTE {left_stack_location}
    sub al, bl
    movsx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left}
    mov {left_stack_location}, ax
{right}
    mov ebx, eax
    mov ax, WORD {left_stack_location}
    sub ax, bx
    movsx rax, ax"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left}
    mov {left_stack_location}, eax
{right}
    mov edi, eax
    mov eax, {left_stack_location}
    sub eax, edi
    cdqe                                  ; rax = sign_extend(eax)"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left}
    mov {left_stack_location}, rax
{right}
    mov rdi, rax
    mov rax, {left_stack_location}
    sub rax, rdi"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left}
    mov {left_stack_location}, eax
{right}
    movss xmm0, DWORD {left_stack_location}
    movd xmm1, eax
    subss xmm0, xmm1
    movd eax, xmm0"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left}
    mov {left_stack_location}, rax
{right}
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
            let expr_size = type_size(expr_type);
            let left_stack_index = stack_index + expr_size;

            let left = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right = compile_expr(right, left_stack_index + expr_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match expr_type {
                Type::I8 => {
                    format!(
                        "{left}
    mov {left_stack_location}, al
{right}
    imul BYTE {left_stack_location}
    movsx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left}
    mov {left_stack_location}, ax
{right}
    imul WORD {left_stack_location}
    movsx rax, ax"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left}
    mov {left_stack_location}, eax
{right}
    imul DWORD {left_stack_location}
    cdqe                                  ; rax = sign_extend(eax)"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left}
    mov {left_stack_location}, rax
{right}
    imul QWORD {left_stack_location}"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left}
    mov {left_stack_location}, eax
{right}
    movd xmm0, eax
    mulss xmm0, DWORD {left_stack_location}
    movd eax, xmm0"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left}
    mov {left_stack_location}, rax
{right}
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
            let expr_size = type_size(expr_type);
            let left_stack_index = stack_index + expr_size;

            let left = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right = compile_expr(right, left_stack_index + expr_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match expr_type {
                Type::I8 => {
                    format!(
                        "{left}
    mov {left_stack_location}, al
{right}
    mov ebx, eax
    movsx eax, BYTE {left_stack_location}
    cdq                                   ; edx = signbit(eax)
    idiv ebx
    movsx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left}
    mov {left_stack_location}, ax
{right}
    mov ebx, eax
    movsx eax, WORD {left_stack_location}
    cdq                                   ; edx = signbit(eax)
    idiv ebx
    movsx rax, ax"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left}
    mov {left_stack_location}, eax
{right}
    mov edi, eax
    mov eax, {left_stack_location}
    cdq                                   ; edx = signbit(eax)
    idiv edi
    cdqe                                  ; rax = sign_extend(eax)"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left}
    mov {left_stack_location}, rax
{right}
    mov rdi, rax
    mov rax, {left_stack_location}
    cdq                                   ; edx = signbit(eax)
    idiv rdi"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left}
    mov {left_stack_location}, eax
{right}
    movss xmm0, DWORD {left_stack_location}
    movd xmm1, eax
    divss xmm0, xmm1
    movd eax, xmm0"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left}
    mov {left_stack_location}, rax
{right}
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
            let left_type = exprs_types.get(&left.id).unwrap();
            let left_size = type_size(left_type);
            let left_stack_index = stack_index + left_size;

            let left_ins = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right_ins =
                compile_expr(right, left_stack_index + left_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match left_type {
                Type::I8 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, al
{right_ins}
    cmp al, {left_stack_location}
    setg al
    movzx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, ax
{right_ins}
    cmp ax, {left_stack_location}
    setg al
    movzx rax, al"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    cmp eax, {left_stack_location}
    setg al
    movzx rax, al"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    cmp rax, {left_stack_location}
    setg al
    movzx rax, al"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    movd xmm0, eax
    comiss xmm0, DWORD {left_stack_location}
    seta al
    movzx rax, al"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    seta al
    movzx rax, al"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::GT(left, right) => {
            let left_type = exprs_types.get(&left.id).unwrap();
            let left_size = type_size(left_type);
            let left_stack_index = stack_index + left_size;

            let left_ins = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right_ins =
                compile_expr(right, left_stack_index + left_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match left_type {
                Type::I8 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, al
{right_ins}
    cmp al, {left_stack_location}
    setl al
    movzx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, ax
{right_ins}
    cmp ax, {left_stack_location}
    setl al
    movzx rax, al"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    cmp eax, {left_stack_location}
    setl al
    movzx rax, al"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    cmp rax, {left_stack_location}
    setl al
    movzx rax, al"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    movd xmm0, eax
    comiss xmm0, DWORD {left_stack_location}
    setb al
    movzx rax, al"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    setb al
    movzx rax, al"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::LE(left, right) => {
            let left_type = exprs_types.get(&left.id).unwrap();
            let left_size = type_size(left_type);
            let left_stack_index = stack_index + left_size;

            let left_ins = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right_ins =
                compile_expr(right, left_stack_index + left_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match left_type {
                Type::I8 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, al
{right_ins}
    cmp al, {left_stack_location}
    setge al
    movzx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, ax
{right_ins}
    cmp ax, {left_stack_location}
    setge al
    movzx rax, al"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    cmp eax, {left_stack_location}
    setge al
    movzx rax, al"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    cmp rax, {left_stack_location}
    setge al
    movzx rax, al"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    movd xmm0, eax
    comiss xmm0, DWORD {left_stack_location}
    setae al
    movzx rax, al"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    setae al
    movzx rax, al"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::GE(left, right) => {
            let left_type = exprs_types.get(&left.id).unwrap();
            let left_size = type_size(left_type);
            let left_stack_index = stack_index + left_size;

            let left_ins = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right_ins =
                compile_expr(right, left_stack_index + left_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match left_type {
                Type::I8 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, al
{right_ins}
    cmp al, {left_stack_location}
    setle al
    movzx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, ax
{right_ins}
    cmp ax, {left_stack_location}
    setle al
    movzx rax, al"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    cmp eax, {left_stack_location}
    setle al
    movzx rax, al"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    cmp rax, {left_stack_location}
    setle al
    movzx rax, al"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    movd xmm0, eax
    comiss xmm0, DWORD {left_stack_location}
    setbe al
    movzx rax, al"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    setbe al
    movzx rax, al"
                    )
                }
                _ => unreachable!(),
            }
        }
        ExprKind::EQ(left, right) => {
            let left_type = exprs_types.get(&left.id).unwrap();
            let left_size = type_size(left_type);
            let left_stack_index = stack_index + left_size;

            let left_ins = compile_expr(left, left_stack_index, false, env, exprs_types);
            let right_ins =
                compile_expr(right, left_stack_index + left_size, false, env, exprs_types);
            let left_stack_location = stack_location(left_stack_index);

            match left_type {
                Type::I8 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, al
{right_ins}
    cmp al, {left_stack_location}
    sete al
    movzx rax, al"
                    )
                }
                Type::I16 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, ax
{right_ins}
    cmp ax, {left_stack_location}
    sete al
    movzx rax, al"
                    )
                }
                Type::I32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    cmp eax, {left_stack_location}
    sete al
    movzx rax, al"
                    )
                }
                Type::I64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    cmp rax, {left_stack_location}
    sete al
    movzx rax, al"
                    )
                }
                Type::F32 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, eax
{right_ins}
    movd xmm0, eax
    comiss xmm0, DWORD {left_stack_location}
    sete al
    movzx rax, al"
                    )
                }
                Type::F64 => {
                    format!(
                        "{left_ins}
    mov {left_stack_location}, rax
{right_ins}
    movq xmm0, rax
    comisd xmm0, QWORD {left_stack_location}
    sete al
    movzx rax, al"
                    )
                }
                _ => unreachable!(),
            }
        }

        // constructs
        ExprKind::If(cond, then, else_) => {
            let else_label = generate_distinct_label("else");
            let end_label = generate_distinct_label("if_end");
            let cond = compile_expr!(cond, false);
            let then = compile_expr!(then, tail_position);
            let else_ = compile_expr!(else_, tail_position);
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
            let last_clause = compile_expr!(last_clause, tail_position);
            let clauses = clauses.iter().fold(String::new(), |clauses, (test, form)| {
                let clause_end_label = generate_distinct_label("clause_end");
                let test = compile_expr!(test, false);
                let form = compile_expr!(form, tail_position);

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
            let cond = compile_expr!(cond, false);
            let body = compile_expr!(body, false);
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
                let value_size = type_size(exprs_types.get(&value.id).unwrap());

                // pad the stack if it wasn't aligned to the current value size
                stack_index += value_size;
                stack_index = (stack_index as f64 / value_size as f64).ceil() as i64 * value_size;

                bindings_ins.push_str(&compile_expr(value, stack_index, false, env, exprs_types));
                bindings_ins.push_str(&format!(
                    "\n    mov {}, {}\n",
                    stack_location(stack_index),
                    register_with_size(value_size)
                ));

                env.push((String::clone(name), Location::Index(stack_index)));
            }

            let body = compile_expr(body, stack_index, tail_position, env, exprs_types);
            env.truncate(previous_bindings_count);

            format!("{bindings_ins}{body}")
        }
        ExprKind::Set(name, value) => {
            let value_ins = compile_expr!(value, false);
            let value_size = type_size(exprs_types.get(&value.id).unwrap());
            format!(
                "{value_ins}\n    mov {}, {}",
                name_location(name, env),
                register_with_size(value_size)
            )
        }
        ExprKind::Seq(first, rest) => {
            let mut seq_ins = compile_expr!(first, false);

            for (i, expr) in rest.iter().enumerate() {
                seq_ins.push('\n');
                seq_ins.push_str(&compile_expr!(expr, tail_position && i == rest.len() - 1));
            }

            seq_ins
        }
        ExprKind::Lambda(parameters, _, body, _captures) => {
            let lambda_label = label_lambda(expr.id);
            let after_lambda_label = generate_distinct_label("after_lambda");
            let previous_env_count = env.len();
            let mut stack_index = 0i64;

            // push the lambda parameters stack index into the environment
            for (parameter_name, parameter_type) in parameters {
                let parameter_size = type_size(parameter_type);

                // pad the stack if it wasn't aligned to the current value size
                stack_index += parameter_size;
                stack_index =
                    (stack_index as f64 / parameter_size as f64).ceil() as i64 * parameter_size;

                env.push((parameter_name.to_owned(), Location::Index(stack_index)));
            }

            // functions body expression is always considered in tail position
            let body = compile_expr(body, stack_index, true, env, exprs_types);

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
        // no-tail call
        ExprKind::App(function, arguments) if tail_position == false => {
            let mut arguments_ins = String::new();
            let mut stack_index = stack_index;

            // the stack index needs to be 16-bytes aligned before the call
            let stack_index_padded = (stack_index as f64 / 16.0).ceil() as i64 * 16;

            // the padding + return address size
            stack_index = stack_index_padded + 8;

            for (i, argument) in arguments.iter().enumerate() {
                let argument_size = type_size(exprs_types.get(&argument.id).unwrap());

                // pad the stack if it wasn't aligned to the current argument size
                stack_index += argument_size;
                stack_index =
                    (stack_index as f64 / argument_size as f64).ceil() as i64 * argument_size;

                let argument_ins = compile_expr(argument, stack_index, false, env, exprs_types);
                arguments_ins.push_str(&format!(
                    "{argument_ins}\n    mov {}, {}",
                    stack_location(stack_index),
                    register_with_size(argument_size)
                ));

                if i != arguments.len() - 1 {
                    arguments_ins.push('\n');
                }
            }

            let function = compile_expr(function, stack_index, false, env, exprs_types);
            let (stack_move, stack_rewind) = if stack_index_padded != 0 {
                (
                    format!(
                        "    ; move rsp to the callee frame\n    sub rsp, {stack_index_padded}"
                    ),
                    format!(
                        "    ; rewind rsp to the current frame\n    add rsp, {stack_index_padded}"
                    ),
                )
            } else {
                (
                    format!("    ; call will take care of moving rsp"),
                    format!("    ; ret took care of rewinding rsp"),
                )
            };

            format!(
                "{arguments_ins}
{function}
{stack_move}
    call rax
{stack_rewind}"
            )
        }
        // tail call
        ExprKind::App(function, arguments) => {
            //
            // To achieve tail call, we simply need to push the arguments onto the
            // start of the current frame, but first we push the arguments on the
            // top of the stack then move it to the start of the current frame as
            // arguments evaluation could reference an existing variable on the stack.
            //
            // This is the state of the frame before the move, assuming a function
            // with three arguments a1, a2 and a3 is being called, and the current
            // frame has two variables v1 and v2, and two arguments b1 and b2:
            //
            //               | a3 | a2 | a1 | v2 | v1 | b2 | b1 | ret_address
            //   stack_idx ----^                          rsp ----^
            //
            // we need to shift a3, a2 and a1 like the following:
            //
            //               | a3 | a2 | a1 | v2 | a3 | a2 | a1 | ret_address
            //                       stack_idx ----^      rsp ----^
            //
            // Note that in this case, we don't need to push any new address we just
            // jump to the called function and it will return to the caller of the
            // current function directly.
            //

            let mut arguments_push_ins = String::new();
            let mut arguments_move_ins = String::new();

            let mut stack_index = stack_index;
            let mut frame_index = 0i64;

            // the instructions to push the arguments onto current frame
            // the instruction to move the arguments to the start of the frame
            for (i, argument) in arguments.iter().enumerate() {
                let argument_size = type_size(exprs_types.get(&argument.id).unwrap());

                stack_index += argument_size;
                frame_index += argument_size;

                // pad the indices if they weren't aligned to the current argument size
                stack_index =
                    (stack_index as f64 / argument_size as f64).ceil() as i64 * argument_size;
                frame_index =
                    (frame_index as f64 / argument_size as f64).ceil() as i64 * argument_size;

                let argument_ins = compile_expr(argument, stack_index, false, env, exprs_types);
                arguments_push_ins.push_str(&format!(
                    "{argument_ins}\n    mov {}, {}",
                    stack_location(stack_index),
                    register_with_size(argument_size)
                ));

                arguments_move_ins.push_str(&format!(
                    "    ; move argument #{i} to beginning of the frame
    mov rbx, {}
    mov {}, rbx",
                    stack_location(stack_index),
                    stack_location(frame_index)
                ));

                if i != arguments.len() - 1 {
                    arguments_push_ins.push('\n');
                    arguments_move_ins.push('\n');
                }
            }

            let function = compile_expr(function, stack_index, false, env, exprs_types);

            format!(
                "    ; tail call
{arguments_push_ins}
{arguments_move_ins}
{function}
    jmp rax"
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
        //  [ possible padding        ]
        //  [ return address          ] <- current rsp
        //  [ argument 1 (most left)  ]
        //  | ...                     |
        //  [ argument N (most right) ]

        match def {
            Def::Fn(name, parameters, _, body) => {
                let previous_env_count = env.len();
                let mut stack_index = 0i64;

                // push the function parameters stack index into the environment
                for (parameter_name, parameter_type) in parameters {
                    let parameter_size = type_size(parameter_type);

                    // pad the stack if it wasn't aligned to the current value size
                    stack_index += parameter_size;
                    stack_index =
                        (stack_index as f64 / parameter_size as f64).ceil() as i64 * parameter_size;

                    env.push((parameter_name.to_owned(), Location::Index(stack_index)));
                }

                // functions body expression is always considered in tail position
                let body = compile_expr(body, stack_index, true, env, exprs_types);

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
        Float32(number) => format!("{}:\n    dd {:e}\n", label_float(expr.id), number),
        Float64(number) => format!("{}:\n    dq {:e}\n", label_float(expr.id), number),
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
    ; move rsp to 16-bytes aligned address
    sub rsp, 8
{}
    ; rewind rsp 16-bytes alignment
    add rsp, 8
    ret",
        compile_data(&prog),
        compile_defs(&prog.definitions, &mut env, exprs_types),
        compile_expr(&prog.expression, 0, false, &mut env, exprs_types)
    )
}
