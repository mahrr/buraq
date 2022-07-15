use crate::parser::{Def, Expr, Prog};

enum Location {
    Index(u32), // index on the stack
    Label,      // label (pointer), indenitifed by the name
}

fn generate_label(label: &str) -> String {
    static mut ID: u64 = 0;
    let id: u64;
    unsafe {
        id = ID;
        ID += 1;
    }
    format!("{}_{}", label, id)
}

fn stack_location(index: u32) -> String {
    let index = index * 8;
    format!("[rsp - {index}]")
}

fn name_location(name: &String, env: &Vec<(String, Location)>) -> String {
    match env.iter().rev().find(|(id, _)| name == id) {
        Some((_, Location::Index(index))) => stack_location(*index),
        Some((name, Location::Label)) => name.to_owned(),
        None => unreachable!(),
    }
}

fn compile_expr(expr: &Expr, stack_index: u32, env: &mut Vec<(String, Location)>) -> String {
    macro_rules! compile_expr {
        ($e:expr) => {
            compile_expr($e, stack_index, env)
        };
    }

    match expr {
        // literals
        Expr::Boolean(true) => String::from("    mov rax, 1"),
        Expr::Boolean(false) => String::from("    mov rax, 0"),
        Expr::Integer(number) => format!("    mov rax, {}", number),

        // identifier
        Expr::Identifier(name) => format!("    mov rax, {}", name_location(name, env)),

        // arithmetics
        Expr::Add(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    add rax, {0}",
                stack_location(stack_index)
            )
        }
        Expr::Sub(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    mov rdi, rax
    mov rax, {0}
    sub rax, rdi",
                stack_location(stack_index)
            )
        }
        Expr::Mul(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    imul QWORD {0}",
                stack_location(stack_index)
            )
        }
        Expr::Div(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    mov rdi, rax
    mov rax, {0}
    xor rdx, rdx
    idiv rdi",
                stack_location(stack_index)
            )
        }

        // comparison
        Expr::LT(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    mov rdi, {0}
    cmp rdi, rax
    setl al
    movzx eax, al",
                stack_location(stack_index)
            )
        }
        Expr::GT(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    mov rdi, {0}
    cmp rdi, rax
    setg al
    movzx eax, al",
                stack_location(stack_index)
            )
        }
        Expr::LE(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    mov rdi, {0}
    cmp rdi, rax
    setle al
    movzx eax, al",
                stack_location(stack_index)
            )
        }
        Expr::GE(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    mov rdi, {0}
    cmp rdi, rax
    setg al
    movzx eax, al",
                stack_location(stack_index)
            )
        }
        Expr::EQ(left, right) => {
            let left = compile_expr(left, stack_index, env);
            let right = compile_expr(right, stack_index + 1, env);
            format!(
                "{left}
    mov {0}, rax
{right}
    mov rdi, {0}
    cmp rdi, rax
    sete al
    movzx eax, al",
                stack_location(stack_index)
            )
        }

        // constructs
        Expr::If(cond, then, else_) => {
            let else_label = generate_label("else");
            let end_label = generate_label("if_end");
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
        Expr::Cond(clauses, last_clause) => {
            let cond_end_label = generate_label("cond_end");
            let last_clause = compile_expr!(last_clause);
            let clauses = clauses.iter().fold(String::new(), |clauses, (test, form)| {
                let clause_end_label = generate_label("clause_end");
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
        Expr::While(cond, body) => {
            let start_label = generate_label("while_start");
            let exit_label = generate_label("while_exit");
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
        Expr::Let(bindings, body) => {
            let previous_bindings_count = env.len();
            let mut bindings_ins = String::new();
            let mut stack_index = stack_index;

            for (name, value) in bindings {
                let value = compile_expr(value, stack_index, env);

                bindings_ins.push_str(&value);
                bindings_ins.push_str(&format!("\n    mov {}, rax\n", stack_location(stack_index)));

                env.push((String::clone(name), Location::Index(stack_index)));
                stack_index += 1;
            }

            let body = compile_expr(body, stack_index, env);
            env.truncate(previous_bindings_count);

            format!("{bindings_ins}{body}")
        }
        Expr::Set(name, value) => {
            let value = compile_expr!(value);
            format!("{value}\n    mov {}, rax", name_location(name, env))
        }
        Expr::Seq(first, rest) => rest.iter().fold(compile_expr!(first), |output, expr| {
            format!("{output}\n{}", compile_expr!(expr))
        }),
        Expr::App(function, arguments) => {
            let after_call_label = generate_label("after_call");
            let function = compile_expr!(function);
            let arguments = arguments
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                    // +2, because the previous RSP and the return address are pushed
                    // into the stack before the called function arguments
                    let stack_index = stack_index + 2 + i as u32;
                    format!(
                        "{0}\n    mov {1}, rax",
                        compile_expr(arg, stack_index, env),
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
    ; move RSP to the caller frame
    jmp rcx
{after_call_label}:
    ; pop previous RSP
    mov rsp, [rsp]",
                stack_location(stack_index),
                stack_location(stack_index + 1),
                (stack_index + 1) * 8 // return address slot
            )
        }
    }
}

fn compile_defs(definitions: &Vec<Def>, env: &mut Vec<(String, Location)>) -> String {
    fn compile_def(def: &Def, env: &mut Vec<(String, Location)>) -> String {
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

                // push the function parameters stack location into the environment
                for i in 0..parameters.len() {
                    let entry = (parameters[i].0.to_owned(), Location::Index(i as u32 + 1));
                    env.push(entry)
                }

                // +1, because the first entry on the stack is reserved for the return address
                let stack_index = parameters.len() as u32 + 1;
                let body = compile_expr(body, stack_index, env);

                env.truncate(previous_env_count);
                format!("{name}:\n{body}\n    ret")
            }
        }
    }

    definitions
        .iter()
        .map(|def| compile_def(def, env))
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn compile(prog: &Prog) -> String {
    // construct the initial environment from the global definitions
    let mut env: Vec<(String, Location)> = prog
        .definitions
        .iter()
        .map(|def| match def {
            Def::Fn(name, _, _, _) => (name.to_owned(), Location::Label),
        })
        .collect();

    format!(
        "    section .text
    global boot
{}
boot:
{}
    ret",
        compile_defs(&prog.definitions, &mut env),
        compile_expr(&prog.expression, 1, &mut env)
    )
}
