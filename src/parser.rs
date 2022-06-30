use crate::sexpr::SExpr;
use std::fmt;

pub enum Expr {
    Boolean(bool),
    Integer(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    LT(Box<Expr>, Box<Expr>),
    GT(Box<Expr>, Box<Expr>),
    LE(Box<Expr>, Box<Expr>),
    GE(Box<Expr>, Box<Expr>),
    EQ(Box<Expr>, Box<Expr>),
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
            if symbol == "true" {
                return Ok(Expr::Boolean(true));
            }
            if symbol == "false" {
                return Ok(Expr::Boolean(false));
            }
            Err(Error)
        }
        SExpr::List(elements) => match &elements[..] {
            [SExpr::Symbol(op), left, right] if op == "+" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Add(left, right))
            }
            [SExpr::Symbol(op), left, right] if op == "-" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Sub(left, right))
            }
            [SExpr::Symbol(op), left, right] if op == "*" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Mul(left, right))
            }
            [SExpr::Symbol(op), left, right] if op == "/" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Div(left, right))
            }
            [SExpr::Symbol(op), left, right] if op == "<" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::LT(left, right))
            }
            [SExpr::Symbol(op), left, right] if op == ">" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::GT(left, right))
            }
            [SExpr::Symbol(op), left, right] if op == "<=" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::LE(left, right))
            }
            [SExpr::Symbol(op), left, right] if op == ">=" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::GE(left, right))
            }
            [SExpr::Symbol(op), left, right] if op == "=" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::EQ(left, right))
            }
            _ => Err(Error),
        },
    }
}
