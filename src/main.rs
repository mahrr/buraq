use std::{
    env,
    fs::{self},
    process,
};

mod sexpr;

fn parse(source: &String) -> Option<i64> {
    let mut result = 0i64;
    let mut chars = source.chars();

    match chars.next() {
        Some(ch) => match ch.to_digit(10) {
            Some(number) => result += number as i64,
            None => return None,
        },
        None => {
            return None;
        }
    };

    while let Some(ch) = chars.next() {
        match ch.to_digit(10) {
            Some(number) => result = result * 10 + number as i64,
            None => break,
        }
    }

    return Some(result);
}

fn compile(number: i64) -> String {
    format!(
        "    section .text
    global boot
boot:
    mov rax, {}
    ret
",
        number
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

            println!("; SEXPR: {:?}", sexpr);

            let number = match sexpr {
                sexpr::SExpr::Symbol(symb) => {
                    if let Some(number) = parse(&symb) {
                        number
                    } else {
                        eprintln!("error: expect a number");
                        process::exit(1);
                    }
                }
                _ => {
                    eprintln!("error: expect a number");
                    process::exit(1);
                }
            };

            print!("{}", compile(number));
        }
        Err(error) => {
            eprintln!("error: {}", error);
            process::exit(1);
        }
    }
}
