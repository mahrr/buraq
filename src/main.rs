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

    // parse the source code into S-Expressions
    let sexprs = match sexpr::parse_multiple(&file_content) {
        Ok(sexprs) => sexprs,
        Err(error) => {
            eprintln!("error: {error}");
            process::exit(1);
        }
    };

    // parse the S-Expression into a Buraq program
    let prog = match parser::parse_prog(&sexprs) {
        Ok(prog) => prog,
        Err(error) => {
            eprintln!("error: {error}");
            process::exit(1);
        }
    };

    // type check the parsed Buraq program
    match type_checker::check(&prog) {
        Ok(_) => (),
        Err(error) => {
            eprintln!("error: {error}");
            process::exit(1);
        }
    }

    // compile the program and dump the result to stdout
    println!("{}", compiler::compile(&prog));
}
