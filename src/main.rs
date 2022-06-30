use std::{
    env,
    fs::{self},
    process,
};

mod parser;
mod sexpr;

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
                "    {}
    push rax
    {}
    pop rbx
    add rax, rbx",
                left, right
            )
        }
        Expr::Sub(left, right) => {
            let left = compile(*left);
            let right = compile(*right);
            format!(
                "    {}
    push rax
    {}
    mov rbx, rax
    pop rax
    sub rax, rbx",
                left, right
            )
        }
        Expr::Mul(left, right) => {
            let left = compile(*left);
            let right = compile(*right);
            format!(
                "    {}
    push rax
    {}
    pop rbx
    mul rbx",
                left, right
            )
        }
        Expr::Div(left, right) => {
            let left = compile(*left);
            let right = compile(*right);
            format!(
                "    {}
    push rax
    {}
    mov rbx, rax
    pop rax
    xor rdx, rdx
    idiv rbx",
                left, right
            )
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

    let file_path = &args[1];

    match fs::read_to_string(file_path) {
        Ok(content) => {
            let sexpr = match sexpr::parse(&content) {
                Ok(sexpr) => sexpr,
                Err(err) => {
                    eprintln!("error: {}", err);
                    process::exit(1);
                }
            };

            match parser::parse(&sexpr) {
                Ok(expr) => println!("{}", compile_program(expr)),
                Err(error) => println!("error: {}", error),
            }
        }
        Err(error) => {
            eprintln!("error: {}", error);
            process::exit(1);
        }
    }
}
