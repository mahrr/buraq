use std::{
    env,
    fs::{self},
    process,
};

mod compiler;
mod parser;
mod sexpr;
mod type_checker;

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

    // type check the parse Buraq expression
    match type_checker::check(&expr) {
        Ok(_) => (),
        Err(error) => {
            eprintln!("error: {error}");
            process::exit(1);
        }
    }

    // compile the expression and dump the result to stdout
    println!("{}", compiler::compile(&expr));
}
