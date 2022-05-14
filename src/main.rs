use std::{
    env,
    fs::{self, File},
    io::Write,
    process,
};

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
        "    section .text:
            global boot
        boot:
            mov rax, {}
            ret
        ",
        number
    )
}

fn dump(file_path: &String, output: &String) -> std::io::Result<()> {
    let mut output_file = File::create(format!("{}.asm", file_path))?;
    let bytes_written = output_file.write(output.as_bytes())?;
    println!("written {} bytes", bytes_written);
    Ok(())
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
            let number = match parse(&content) {
                Some(number) => number,
                None => {
                    eprintln!("parse error: invalid number");
                    process::exit(1);
                }
            };

            match dump(file_path, &compile(number)) {
                Ok(()) => {}
                Err(error) => {
                    eprintln!("io error: {}", error);
                    process::exit(1);
                }
            }
        }
        Err(error) => {
            eprintln!("io error: {}", error);
            process::exit(1);
        }
    }
}
