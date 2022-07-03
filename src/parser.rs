use crate::sexpr::SExpr;
use std::fmt;

pub enum Expr {
    Boolean(bool),
    Integer(i64),
    Add(Box<Expr>, Box<Expr>), // left, right
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    LT(Box<Expr>, Box<Expr>),
    GT(Box<Expr>, Box<Expr>),
    LE(Box<Expr>, Box<Expr>),
    GE(Box<Expr>, Box<Expr>),
    EQ(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>), // cond, then, else
    Let(Vec<(String, Expr)>, Box<Expr>), // (vars, vals), body
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
            // arithmetics
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

            // comparison
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

            // if
            [SExpr::Symbol(keyword), cond, then, else_] if keyword == "if" => {
                let cond = Box::new(parse(cond)?);
                let then = Box::new(parse(then)?);
                let else_ = Box::new(parse(else_)?);
                Ok(Expr::If(cond, then, else_))
            }

            // let
            [SExpr::Symbol(keyword), SExpr::List(bindings), body] if keyword == "let" => {
                let body = Box::new(parse(body)?);
                let bindings =
                    bindings
                        .iter()
                        .try_fold(vec![], |mut bindings, sexpr| match sexpr {
                            SExpr::List(elements) => match &elements[..] {
                                [SExpr::Symbol(name), value] => {
                                    bindings.push((name.to_owned(), parse(value)?));
                                    Ok(bindings)
                                }
                                _ => Err(Error),
                            },
                            _ => Err(Error),
                        })?;

                Ok(Expr::Let(bindings, body))
            }
            _ => Err(Error),
        },
    }
}
