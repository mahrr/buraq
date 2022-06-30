use std::fmt;
use std::iter::Peekable;

// S-Expression Parser

#[derive(Debug)]
pub enum SExpr {
    Symbol(String),
    List(Vec<SExpr>),
}

#[derive(Debug, Clone)]
pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid s-expression")
    }
}

fn is_delimiter(ch: char) -> bool {
    ch == '(' || ch == ')' || ch.is_ascii_whitespace()
}

fn consume_whitespace<I: Iterator<Item = char>>(it: &mut Peekable<I>) {
    while let Some(_) = it.next_if(|ch| ch.is_ascii_whitespace()) {}
}

fn parse_impl<I: Iterator<Item = char>>(it: &mut Peekable<I>) -> Result<SExpr, Error> {
    consume_whitespace(it);

    match it.peek() {
        Some('(') => {
            it.next(); // consume '('

            let mut elements = vec![];
            loop {
                consume_whitespace(it);
                if let Some(_) = it.next_if_eq(&')') {
                    break;
                }
                elements.push(parse_impl(it)?);
            }
            Ok(SExpr::List(elements))
        }
        Some(_) => {
            let mut symbol: String = String::new();
            while let Some(ch) = it.next_if(|&ch| !is_delimiter(ch)) {
                symbol.push(ch);
            }

            if symbol.len() == 0 {
                return Err(Error);
            }
            Ok(SExpr::Symbol(symbol))
        }
        None => Err(Error),
    }
}

pub fn parse(source: &String) -> Result<SExpr, Error> {
    parse_impl(&mut source.chars().peekable())
}
