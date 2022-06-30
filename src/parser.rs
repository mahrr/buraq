use crate::sexpr::SExpr;
use std::fmt;

pub enum Expr {
    Integer(i64),
    Add(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid expression")
    }
}

fn parse_integer(source: &String) -> Option<i64> {
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

pub fn parse(sexpr: &SExpr) -> Result<Expr, Error> {
    match sexpr {
        SExpr::Symbol(symbol) => {
            if let Some(number) = parse_integer(&symbol) {
                return Ok(Expr::Integer(number));
            }
            Err(Error)
        }
        SExpr::List(elements) => match &elements[..] {
            [SExpr::Symbol(op), left, right] if op == "+" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Add(left, right))
            }
            _ => Err(Error),
        },
    }
}
