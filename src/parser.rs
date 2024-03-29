use crate::sexpr::SExpr;
use crate::type_checker::Type;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Boolean(bool),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
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
    Lambda(Vec<(String, Type)>, Type, Box<Expr>, Vec<String>), // parameters, return_type, body, captures
    App(Box<Expr>, Vec<Expr>),                                 // function, arguments
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub id: u64,
    pub kind: ExprKind,
}

pub enum Def {
    Fn(String, Vec<(String, Type)>, Type, Expr), // name, parameters, return_type, body
}

pub struct Prog {
    pub definitions: Vec<Def>,
    pub expression: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidExpr,
    InvalidDef,
    InvalidType,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidExpr => write!(f, "invalid expression"),
            Error::InvalidDef => write!(f, "invalid definition"),
            Error::InvalidType => write!(f, "invalid type annotation"),
        }
    }
}

fn generate_id() -> u64 {
    static mut ID: u64 = 0;
    let id: u64;
    unsafe {
        id = ID;
        ID += 1;
    }
    id
}

fn parse_float(source: &String) -> Option<ExprKind> {
    let mut sign = 1f64;
    let mut result = 0f64;
    let mut bit_size = 0i64; // 0 indicates no size (default to f64)
    let mut chars = source.chars().peekable();

    if let Some('-') = chars.peek() {
        chars.next(); // consume `-`
        sign = -1f64;
    }

    while let Some(ch) = chars.peek() {
        match ch.to_digit(10) {
            Some(number) => {
                result = result * 10.0 + number as f64;
                chars.next();
            }
            None => break,
        }
    }

    if let Some('.') = chars.peek() {
        chars.next(); // consume `.`
    } else {
        return None;
    }

    let mut fraction_digit = 10f64;
    let mut consuming_bit_size = false;

    while let Some(ch) = chars.next() {
        if ch == 'f' {
            consuming_bit_size = true;
            continue;
        }

        match ch.to_digit(10) {
            Some(number) => {
                if consuming_bit_size == false {
                    result += (number as f64) / fraction_digit;
                    fraction_digit *= 10.0;
                } else {
                    bit_size = bit_size * 10 + number as i64;
                }
            }
            None => return None,
        }
    }

    match bit_size {
        0 | 64 => Some(ExprKind::Float64(sign * result)),
        32 => Some(ExprKind::Float32((sign * result) as f32)),
        _ => None,
    }
}

fn parse_integer(source: &String) -> Option<ExprKind> {
    let mut sign = 1i64;
    let mut result = 0i64;
    let mut bit_size = 0i64; // 0 indicates no size (default to i64)
    let mut chars = source.chars().peekable();

    if let Some('-') = chars.peek() {
        chars.next(); // consume `-`
        sign = -1i64;
    }

    match chars.next() {
        Some(ch) => match ch.to_digit(10) {
            Some(number) => result += number as i64,
            None => return None,
        },
        None => {
            return None;
        }
    };

    // TODO: check overflow
    let mut consuming_bit_size = false;
    while let Some(ch) = chars.next() {
        if ch == 'i' {
            consuming_bit_size = true;
            continue;
        }

        match ch.to_digit(10) {
            Some(number) => {
                if consuming_bit_size == false {
                    result = result * 10 + number as i64;
                } else {
                    bit_size = bit_size * 10 + number as i64;
                }
            }
            None => return None,
        }
    }

    match bit_size {
        0 | 64 => Some(ExprKind::Int64(sign * result)),
        8 => Some(ExprKind::Int8((sign * result) as i8)),
        16 => Some(ExprKind::Int16((sign * result) as i16)),
        32 => Some(ExprKind::Int32((sign * result) as i32)),
        _ => None,
    }
}

fn parse_type(sexpr: &SExpr) -> Result<Type, Error> {
    fn parse_parameters_types(sexpr: &SExpr) -> Result<Vec<Type>, Error> {
        match sexpr {
            SExpr::Symbol(_) => Ok(vec![parse_type(sexpr)?]),
            SExpr::List(types) => {
                let types = types.iter().try_fold(vec![], |mut types, sexpr| {
                    types.push(parse_type(sexpr)?);
                    Ok(types)
                })?;
                Ok(types)
            }
        }
    }

    match sexpr {
        // Literal Types
        SExpr::Symbol(keyword) if keyword == "i8" => Ok(Type::I8),
        SExpr::Symbol(keyword) if keyword == "i16" => Ok(Type::I16),
        SExpr::Symbol(keyword) if keyword == "i32" => Ok(Type::I32),
        SExpr::Symbol(keyword) if keyword == "i64" => Ok(Type::I64),
        SExpr::Symbol(keyword) if keyword == "f64" => Ok(Type::F64),
        SExpr::Symbol(keyword) if keyword == "bool" => Ok(Type::Boolean),
        // Function Type
        SExpr::List(elements) => match &elements[..] {
            [parameters_types, SExpr::Symbol(delimiter), return_type] if delimiter == "->" => {
                let parameters = parse_parameters_types(parameters_types)?;
                let return_type = parse_type(return_type)?;
                Ok(Type::Fn(parameters, Box::new(return_type)))
            }
            _ => Err(Error::InvalidType),
        },
        _ => Err(Error::InvalidType),
    }
}

fn parse_parameters(sexprs: &Vec<SExpr>) -> Result<Vec<(String, Type)>, Error> {
    use SExpr::*;

    // check if it's a one parameter in the form: (<name> <type>)
    if sexprs.len() == 2 {
        match (&sexprs[0], &sexprs[1]) {
            (Symbol(name), type_) => return Ok(vec![(name.to_owned(), parse_type(type_)?)]),
            _ => {}
        }
    }

    sexprs
        .iter()
        .try_fold(vec![], |mut parameters, sexpr| match sexpr {
            List(elements) => match &elements[..] {
                [Symbol(name), type_] => {
                    parameters.push((name.to_owned(), parse_type(type_)?));
                    Ok(parameters)
                }
                _ => Err(Error::InvalidDef),
            },
            Symbol(_) => Err(Error::InvalidDef),
        })
}

pub fn parse_expr(sexpr: &SExpr) -> Result<Expr, Error> {
    use SExpr::*;

    macro_rules! E {
        ($kind:expr) => {
            Ok(Expr {
                id: generate_id(),
                kind: $kind,
            })
        };
    }

    match sexpr {
        Symbol(symbol) => {
            if let Some(expr) = parse_float(&symbol) {
                return E!(expr);
            }
            if let Some(expr) = parse_integer(&symbol) {
                return E!(expr);
            }
            if symbol == "true" {
                return E!(ExprKind::Boolean(true));
            }
            if symbol == "false" {
                return E!(ExprKind::Boolean(false));
            }
            E!(ExprKind::Identifier(symbol.to_owned()))
        }
        List(elements) => match &elements[..] {
            // arithmetics
            [Symbol(op), left, right] if op == "+" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::Add(left, right))
            }
            [Symbol(op), left, right] if op == "-" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::Sub(left, right))
            }
            [Symbol(op), left, right] if op == "*" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::Mul(left, right))
            }
            [Symbol(op), left, right] if op == "/" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::Div(left, right))
            }

            // comparison
            [Symbol(op), left, right] if op == "<" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::LT(left, right))
            }
            [Symbol(op), left, right] if op == ">" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::GT(left, right))
            }
            [Symbol(op), left, right] if op == "<=" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::LE(left, right))
            }
            [Symbol(op), left, right] if op == ">=" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::GE(left, right))
            }
            [Symbol(op), left, right] if op == "=" => {
                let left = Box::new(parse_expr(left)?);
                let right = Box::new(parse_expr(right)?);
                E!(ExprKind::EQ(left, right))
            }

            // constructs
            [Symbol(keyword), cond, then, else_] if keyword == "if" => {
                let cond = Box::new(parse_expr(cond)?);
                let then = Box::new(parse_expr(then)?);
                let else_ = Box::new(parse_expr(else_)?);
                E!(ExprKind::If(cond, then, else_))
            }
            [Symbol(keyword), clauses @ .., List(last_clause)] if keyword == "cond" => {
                let last_clause = match &last_clause[..] {
                    [Symbol(keyword), form] if keyword == "else" => parse_expr(form),
                    _ => Err(Error::InvalidExpr),
                }?;

                let clauses =
                    clauses
                        .iter()
                        .try_fold(vec![], |mut clauses, sexpr| match sexpr {
                            List(pair) => match &pair[..] {
                                [test, form] => {
                                    clauses.push((parse_expr(test)?, parse_expr(form)?));
                                    Ok(clauses)
                                }
                                _ => Err(Error::InvalidExpr),
                            },
                            _ => Err(Error::InvalidExpr),
                        })?;

                E!(ExprKind::Cond(clauses, Box::new(last_clause)))
            }
            [Symbol(keyword), cond, body] if keyword == "while" => {
                let cond = Box::new(parse_expr(cond)?);
                let body = Box::new(parse_expr(body)?);
                E!(ExprKind::While(cond, body))
            }
            [Symbol(keyword), List(bindings), body] if keyword == "let" => {
                let body = Box::new(parse_expr(body)?);
                let bindings =
                    bindings
                        .iter()
                        .try_fold(vec![], |mut bindings, sexpr| match sexpr {
                            List(elements) => match &elements[..] {
                                [Symbol(name), value] => {
                                    bindings.push((name.to_owned(), parse_expr(value)?));
                                    Ok(bindings)
                                }
                                _ => Err(Error::InvalidExpr),
                            },
                            _ => Err(Error::InvalidExpr),
                        })?;

                E!(ExprKind::Let(bindings, body))
            }
            [Symbol(keyword), Symbol(name), value] if keyword == "set" => {
                E!(ExprKind::Set(name.to_owned(), Box::new(parse_expr(value)?)))
            }
            [Symbol(keyword), first, rest @ ..] if keyword == "seq" => {
                let first = Box::new(parse_expr(first)?);
                let rest = rest.iter().try_fold(vec![], |mut rest, sexpr| {
                    rest.push(parse_expr(sexpr)?);
                    Ok(rest)
                })?;
                E!(ExprKind::Seq(first, rest))
            }
            [Symbol(keyword), List(parameters), return_type, body] if keyword == "lam" => {
                let parameters = parse_parameters(parameters)?;
                let return_type = parse_type(return_type)?;
                let body = Box::new(parse_expr(body)?);
                E!(ExprKind::Lambda(parameters, return_type, body, vec![],))
            }
            [function, arguments @ ..] => {
                let function = Box::new(parse_expr(function)?);
                let arguments = arguments.iter().try_fold(vec![], |mut arguments, sexpr| {
                    arguments.push(parse_expr(sexpr)?);
                    Ok(arguments)
                })?;
                E!(ExprKind::App(function, arguments))
            }
            _ => Err(Error::InvalidExpr),
        },
    }
}

pub fn parse_def(sexpr: &SExpr) -> Result<Def, Error> {
    use SExpr::*;

    match sexpr {
        List(elements) => match &elements[..] {
            [Symbol(keyword), Symbol(name), List(parameters), return_type, body]
                if keyword == "defn" =>
            {
                let parameters = parse_parameters(parameters)?;
                let return_type = parse_type(return_type)?;
                let body = parse_expr(body)?;
                Ok(Def::Fn(name.to_owned(), parameters, return_type, body))
            }
            _ => Err(Error::InvalidDef),
        },
        Symbol(_) => Err(Error::InvalidDef),
    }
}

pub fn parse_prog(sexprs: &Vec<SExpr>) -> Result<Prog, Error> {
    let mut definitions = vec![];
    let expression: Expr;

    match &sexprs[..] {
        [sexpr] => expression = parse_expr(sexpr)?,
        [defs @ .., sexpr] => {
            for def in defs {
                definitions.push(parse_def(def)?);
            }
            expression = parse_expr(sexpr)?;
        }
        _ => unreachable!(),
    }

    Ok(Prog {
        expression: expression,
        definitions: definitions,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expr_literals() {
        fn assert_symbol(sym: &str, kind: ExprKind) {
            let sexpr = SExpr::Symbol(sym.to_string());
            let expr = parse_expr(&sexpr).unwrap();
            assert_eq!(expr.kind, kind);
        }

        // booleans
        assert_symbol("true", ExprKind::Boolean(true));
        assert_symbol("false", ExprKind::Boolean(false));

        // integers
        assert_symbol("25", ExprKind::Int64(25));
        assert_symbol("25i8", ExprKind::Int8(25));
        assert_symbol("25i16", ExprKind::Int16(25));
        assert_symbol("25i32", ExprKind::Int32(25));
        assert_symbol("25i64", ExprKind::Int64(25));
        assert_symbol("-25", ExprKind::Int64(-25));
        assert_symbol("-25i8", ExprKind::Int8(-25));
        assert_symbol("-25i16", ExprKind::Int16(-25));
        assert_symbol("-25i32", ExprKind::Int32(-25));
        assert_symbol("-25i64", ExprKind::Int64(-25));

        // floats
        assert_symbol("25.0", ExprKind::Float64(25.0));
        assert_symbol("25.", ExprKind::Float64(25.0));
        assert_symbol("25.25", ExprKind::Float64(25.25));
        assert_symbol("0.25", ExprKind::Float64(0.25));
        assert_symbol(".25", ExprKind::Float64(0.25));
        assert_symbol("-25.0", ExprKind::Float64(-25.0));
        assert_symbol("-25.", ExprKind::Float64(-25.0));
        assert_symbol("-25.25", ExprKind::Float64(-25.25));
        assert_symbol("-0.25", ExprKind::Float64(-0.25));
        assert_symbol("-.25", ExprKind::Float64(-0.25));
        assert_symbol("25.25f32", ExprKind::Float32(25.25));
        assert_symbol("0.25f32", ExprKind::Float32(0.25));
        assert_symbol(".25f32", ExprKind::Float32(0.25));
        assert_symbol("-25.0f32", ExprKind::Float32(-25.0));
        assert_symbol("-25.f32", ExprKind::Float32(-25.0));
        assert_symbol("-25.25f32", ExprKind::Float32(-25.25));
        assert_symbol("-0.25f32", ExprKind::Float32(-0.25));
        assert_symbol("-.25f32", ExprKind::Float32(-0.25));
        assert_symbol("25.25f64", ExprKind::Float64(25.25));
        assert_symbol("0.25f64", ExprKind::Float64(0.25));
        assert_symbol(".25f64", ExprKind::Float64(0.25));
        assert_symbol("-25.0f64", ExprKind::Float64(-25.0));
        assert_symbol("-25.f64", ExprKind::Float64(-25.0));
        assert_symbol("-25.25f64", ExprKind::Float64(-25.25));
        assert_symbol("-0.25f64", ExprKind::Float64(-0.25));
        assert_symbol("-.25f64", ExprKind::Float64(-0.25));
    }
}
