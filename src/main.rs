use std::{
    env,
    fs::{self},
    process,
};

mod parser;
mod sexpr;

fn generate_label(label: &str) -> String {
    static mut ID: u64 = 0;
    let id: u64;
    unsafe {
        id = ID;
        ID += 1;
    }
    format!("{}_{}", label, id)
}

fn compile(expr: parser::Expr) -> String {
    use parser::*;

    match expr {
        // literals
        Expr::Boolean(true) => String::from("mov rax, 1"),
        Expr::Boolean(false) => String::from("mov rax, 0"),
        Expr::Integer(number) => format!("mov rax, {}", number),

        // arithmetics
        Expr::Add(left, right) => {
            let left = compile(*left);
            let right = compile(*right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    add rax, rbx"
            )
        }
        Expr::Sub(left, right) => {
            let left = compile(*left);
            let right = compile(*right);
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
            let left = compile(*left);
            let right = compile(*right);
            format!(
                "    {left}
    push rax
    {right}
    pop rbx
    mul rbx"
            )
        }
        Expr::Div(left, right) => {
            let left = compile(*left);
            let right = compile(*right);
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
            let left = compile(*left);
            let right = compile(*right);
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
            let left = compile(*left);
            let right = compile(*right);
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
            let left = compile(*left);
            let right = compile(*right);
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
            let left = compile(*left);
            let right = compile(*right);
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
            let left = compile(*left);
            let right = compile(*right);
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
            let cond = compile(*cond);
            let then = compile(*then);
            let else_ = compile(*else_);
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

fn compile_program(expr: parser::Expr) -> String {
    format!(
        "    section .text
    global boot
boot:
{}
    ret",
        compile(expr)
    )
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("usage: boruq <file-name>");
        process::exit(1);
    }

    // read file content
    let file_path = &args[1];
    let file_content = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(error) => {
            eprintln!("error: {error}");
            process::exit(1);
        }
    };

    // parse the source code into an S-Expression
    let sexpr = match sexpr::parse(&file_content) {
        Ok(sexpr) => sexpr,
        Err(error) => {
            eprintln!("error: {error}");
            process::exit(1);
        }
    };

    // parse the S-Expression into a Buraq expression
    let expr = match parser::parse(&sexpr) {
        Ok(expr) => expr,
        Err(error) => {
            eprintln!("error: {error}");
            process::exit(1);
        }
    };

    // compile the expression and dump the result to stdout
    println!("{}", compile_program(expr));
}
