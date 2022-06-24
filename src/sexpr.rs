use std::fmt;
use std::iter::Peekable;

// S-Expression Parser

#[derive(Debug)]
pub enum SExpr {
    Symbol(String),
    List(Vec<SExpr>),
}

fn is_delimiter(ch: char) -> bool {
    match ch {
        '(' | ')' => true,
        _ => ch.is_ascii_whitespace(),
    }
}

fn consume_whitespace<I: Iterator<Item = char>>(it: &mut Peekable<I>) {
    while let Some(_) = it.next_if(|ch| ch.is_ascii_whitespace()) {}
}

#[derive(Debug, Clone)]
pub struct ParseError;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse Error")
    }
}

fn parse_impl<I: Iterator<Item = char>>(it: &mut Peekable<I>) -> Result<SExpr, ParseError> {
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

                let expr = parse_impl(it)?;
                elements.push(expr);
            }
            Ok(SExpr::List(elements))
        }
        Some(_) => {
            let mut symbol: String = String::new();
            while let Some(ch) = it.next_if(|&ch| !is_delimiter(ch)) {
                symbol.push(ch);
            }

            if symbol.len() == 0 {
                return Err(ParseError);
            }
            Ok(SExpr::Symbol(symbol))
        }
        None => Err(ParseError),
    }
}

pub fn parse(source: &String) -> Result<SExpr, ParseError> {
    parse_impl(&mut source.chars().peekable())
}
