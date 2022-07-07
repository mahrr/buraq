use crate::parser::Expr;

#[derive(Debug)]
struct GenState {
    stack_index: u32,
    environment: Vec<(String, u32)>,
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

fn compile_expr(state: &mut GenState, expr: &Expr) -> String {
    match expr {
        // literals
        Expr::Boolean(true) => String::from("mov rax, 1"),
        Expr::Boolean(false) => String::from("mov rax, 0"),
        Expr::Integer(number) => format!("mov rax, {}", number),

        // arithmetics
        Expr::Add(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    add rax, rbx"
            )
        }
        Expr::Sub(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    mov rbx, rax
    pop rax
    sub rax, rbx"
            )
        }
        Expr::Mul(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    mul rbx"
            )
        }
        Expr::Div(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    mov rbx, rax
    pop rax
    xor rdx, rdx
    idiv rbx"
            )
        }

        // comparison
        Expr::LT(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmovl rax, rbx"
            )
        }
        Expr::GT(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmovg rax, rbx"
            )
        }
        Expr::LE(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmovle rax, rbx"
            )
        }
        Expr::GE(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmovge rax, rbx"
            )
        }
        Expr::EQ(left, right) => {
            let left = compile_expr(state, left);
            let right = compile_expr(state, right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    cmp rbx, rax
    mov rax, 0
    mov rbx, 1
    cmove rax, rbx"
            )
        }

        // if
        Expr::If(cond, then, else_) => {
            let else_label = generate_label("else");
            let end_label = generate_label("if_end");
            let cond = compile_expr(state, cond);
            let then = compile_expr(state, then);
            let else_ = compile_expr(state, else_);
            format!(
                "    {cond}
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
        Expr::Let(_bindings, _body) => {
            todo!()
        }
    }
}

pub fn compile(expr: &Expr) -> String {
    let mut state = GenState {
        stack_index: 0,
        environment: vec![],
    };

    format!(
        "    section .text
    global boot
boot:
{}
    ret",
        compile_expr(&mut state, expr)
    )
}
