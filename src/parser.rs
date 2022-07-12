use crate::sexpr::SExpr;
use std::fmt;

pub enum Expr {
    Boolean(bool),
    Integer(i64),
    Identifier(String),
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
    Cond(Vec<(Expr, Expr)>, Box<Expr>),  // variants, else
    While(Box<Expr>, Box<Expr>),         // cond, body
    Let(Vec<(String, Expr)>, Box<Expr>), // (vars, vals), body
    Set(String, Box<Expr>),              // var, val
    Seq(Box<Expr>, Vec<Expr>),           // first, ..rest
}

pub enum Def {
    Fn(String, Vec<String>, Expr),
}

#[derive(Debug, Clone)]
pub enum Error {
    InvalidExpr,
    InvalidDef,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidExpr => write!(f, "invalid expression"),
            Error::InvalidDef => write!(f, "invalid definition"),
        }
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
    use SExpr::*;

    match sexpr {
        Symbol(symbol) => {
            if let Some(number) = parse_integer(&symbol) {
                return Ok(Expr::Integer(number));
            }
            if symbol == "true" {
                return Ok(Expr::Boolean(true));
            }
            if symbol == "false" {
                return Ok(Expr::Boolean(false));
            }
            Ok(Expr::Identifier(symbol.to_owned()))
        }
        List(elements) => match &elements[..] {
            // arithmetics
            [Symbol(op), left, right] if op == "+" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Add(left, right))
            }
            [Symbol(op), left, right] if op == "-" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Sub(left, right))
            }
            [Symbol(op), left, right] if op == "*" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Mul(left, right))
            }
            [Symbol(op), left, right] if op == "/" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::Div(left, right))
            }

            // comparison
            [Symbol(op), left, right] if op == "<" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::LT(left, right))
            }
            [Symbol(op), left, right] if op == ">" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::GT(left, right))
            }
            [Symbol(op), left, right] if op == "<=" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::LE(left, right))
            }
            [Symbol(op), left, right] if op == ">=" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::GE(left, right))
            }
            [Symbol(op), left, right] if op == "=" => {
                let left = Box::new(parse(left)?);
                let right = Box::new(parse(right)?);
                Ok(Expr::EQ(left, right))
            }

            // constructs
            [Symbol(keyword), cond, then, else_] if keyword == "if" => {
                let cond = Box::new(parse(cond)?);
                let then = Box::new(parse(then)?);
                let else_ = Box::new(parse(else_)?);
                Ok(Expr::If(cond, then, else_))
            }
            [Symbol(keyword), clauses @ .., List(last_clause)] if keyword == "cond" => {
                let last_clause = match &last_clause[..] {
                    [Symbol(keyword), form] if keyword == "else" => parse(form),
                    _ => Err(Error::InvalidExpr),
                }?;

                let clauses =
                    clauses
                        .iter()
                        .try_fold(vec![], |mut clauses, sexpr| match sexpr {
                            List(pair) => match &pair[..] {
                                [test, form] => {
                                    clauses.push((parse(test)?, parse(form)?));
                                    Ok(clauses)
                                }
                                _ => Err(Error::InvalidExpr),
                            },
                            _ => Err(Error::InvalidExpr),
                        })?;

                Ok(Expr::Cond(clauses, Box::new(last_clause)))
            }
            [Symbol(keyword), cond, body] if keyword == "while" => {
                let cond = Box::new(parse(cond)?);
                let body = Box::new(parse(body)?);
                Ok(Expr::While(cond, body))
            }
            [Symbol(keyword), List(bindings), body] if keyword == "let" => {
                let body = Box::new(parse(body)?);
                let bindings =
                    bindings
                        .iter()
                        .try_fold(vec![], |mut bindings, sexpr| match sexpr {
                            List(elements) => match &elements[..] {
                                [Symbol(name), value] => {
                                    bindings.push((name.to_owned(), parse(value)?));
                                    Ok(bindings)
                                }
                                _ => Err(Error::InvalidExpr),
                            },
                            _ => Err(Error::InvalidExpr),
                        })?;

                Ok(Expr::Let(bindings, body))
            }
            [Symbol(keyword), Symbol(name), value] if keyword == "set" => {
                Ok(Expr::Set(name.to_owned(), Box::new(parse(value)?)))
            }
            [Symbol(keyword), first, rest @ ..] if keyword == "seq" => {
                let first = Box::new(parse(first)?);
                let rest = rest.iter().try_fold(vec![], |mut rest, sexpr| {
                    rest.push(parse(sexpr)?);
                    Ok(rest)
                })?;
                Ok(Expr::Seq(first, rest))
            }
            _ => Err(Error::InvalidExpr),
        },
    }
}

pub fn parse_def(sexpr: &SExpr) -> Result<Def, Error> {
    use SExpr::*;

    match sexpr {
        List(elements) => match &elements[..] {
            [Symbol(keyword), Symbol(name), List(parameters), body] if keyword == "defn" => {
                let body = parse(body)?;
                let parameters = parameters
                    .iter()
                    .try_fold(vec![], |mut parameters, sexpr| match sexpr {
                        Symbol(parameter) => {
                            parameters.push(parameter.to_owned());
                            Ok(parameters)
                        }
                        List(_) => Err(Error::InvalidDef),
                    })?;
                Ok(Def::Fn(name.to_owned(), parameters, body))
            }
            _ => Err(Error::InvalidDef),
        },
        Symbol(_) => Err(Error::InvalidDef),
    }
}
