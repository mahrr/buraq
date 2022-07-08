use crate::parser::Expr;

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

fn compile_expr(expr: &Expr, stack_index: u32, env: &mut Vec<(String, u32)>) -> String {
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
        Expr::Identifier(name) => match env.iter().rev().find(|(id, _)| name == id) {
            Some((_, index)) => format!("    mov rax, {}", stack_location(*index)),
            None => unreachable!() // should be caught at a previous phase,
        },

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
    mov rbx, rax
    mov rax, {0}
    sub rax, rbx",
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
    mov rbx, rax
    mov rax, {0}
    xor rdx, rdx
    idiv rbx",
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
    mov rbx, {0}
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmovl rax, rbx",
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
    mov rbx, {0}
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmovg rax, rbx",
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
    mov rbx, {0}
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmovle rax, rbx",
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
    mov rbx, {0}
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmovge rax, rbx",
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
    mov rbx, {0}
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmove rax, rbx",
                stack_location(stack_index)
            )
        }

        // if
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

        // let
        Expr::Let(bindings, body) => {
            let previous_bindings_count = env.len();
            let mut bindings_ins = String::new();
            let mut stack_index = stack_index;

            for (name, value) in bindings {
                let value = compile_expr(value, stack_index, env);

                bindings_ins.push_str(&value);
                bindings_ins.push_str(&format!("\n    mov {}, rax\n", stack_location(stack_index)));

                env.push((String::clone(name), stack_index));
                stack_index += 1;
            }

            let body = compile_expr(body, stack_index, env);
            env.truncate(previous_bindings_count);

            format!(
                ";; bindings
{bindings_ins};; body
{body}"
            )
        }
    }
}

pub fn compile(expr: &Expr) -> String {
    let mut env = vec![];

    format!(
        "    section .text
    global boot
boot:
{}
    ret",
        compile_expr(expr, 1, &mut env)
    )
}
