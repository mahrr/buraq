use crate::parser::Expr;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    TypeMismatch,
    UnboundName(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::TypeMismatch => write!(f, "type mismatch"),
            Error::UnboundName(name) => write!(f, "unbound name '{name}'"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    I64,
    Boolean,
}

fn name_type(name: &String, env: &Vec<(String, Type)>) -> Result<Type, Error> {
    match env.iter().rev().find(|(id, _)| name == id) {
        Some((_, type_)) => Ok(*type_),
        _ => Err(Error::UnboundName(name.to_owned())),
    }
}

fn check_impl(expr: &Expr, env: &mut Vec<(String, Type)>) -> Result<Type, Error> {
    macro_rules! tc_arithmetic {
        ($left:expr, $right:expr) => {{
            let left = check_impl($left, env)?;
            let right = check_impl($right, env)?;

            match (left, right) {
                (Type::I64, Type::I64) => Ok(Type::I64),
                _ => Err(Error::TypeMismatch),
            }
        }};
    }

    macro_rules! tc_comparison {
        ($left:expr, $right:expr) => {{
            let left = check_impl($left, env)?;
            let right = check_impl($right, env)?;

            match (left, right) {
                (Type::I64, Type::I64) => Ok(Type::Boolean),
                _ => Err(Error::TypeMismatch),
            }
        }};
    }

    match expr {
        Expr::Boolean(_) => Ok(Type::Boolean),
        Expr::Integer(_) => Ok(Type::I64),
        Expr::Identifier(name) => name_type(name, env),
        Expr::Add(left, right) => tc_arithmetic!(left, right),
        Expr::Sub(left, right) => tc_arithmetic!(left, right),
        Expr::Mul(left, right) => tc_arithmetic!(left, right),
        Expr::Div(left, right) => tc_arithmetic!(left, right),
        Expr::LT(left, right) => tc_comparison!(left, right),
        Expr::GT(left, right) => tc_comparison!(left, right),
        Expr::LE(left, right) => tc_comparison!(left, right),
        Expr::GE(left, right) => tc_comparison!(left, right),
        Expr::EQ(left, right) => tc_comparison!(left, right),
        Expr::If(cond, then, else_) => {
            let cond = check_impl(cond, env)?;
            let then = check_impl(then, env)?;
            let else_ = check_impl(else_, env)?;

            if cond == Type::Boolean && then == else_ {
                Ok(then)
            } else {
                Err(Error::TypeMismatch)
            }
        }
        Expr::Cond(clauses, last_clause) => {
            let last_clause = check_impl(last_clause, env)?;

            clauses
                .iter()
                .try_fold(last_clause, |last_clause, (test, form)| {
                    let test = check_impl(test, env)?;
                    let form = check_impl(form, env)?;

                    if test == Type::Boolean && form == last_clause {
                        Ok(last_clause)
                    } else {
                        Err(Error::TypeMismatch)
                    }
                })
        }
        Expr::Let(bindings, body) => {
            let prev_bindings_count = env.len();

            for (name, value) in bindings {
                let value = check_impl(value, env)?;
                env.push((name.to_owned(), value));
            }

            let body = check_impl(body, env);
            env.truncate(prev_bindings_count);
            body
        }
        Expr::Set(name, value) => {
            let name = name_type(name, env)?;
            let value = check_impl(value, env)?;
            if name == value {
                Ok(name)
            } else {
                Err(Error::TypeMismatch)
            }
        }
        Expr::Seq(first, rest) => rest
            .iter()
            .try_fold(check_impl(first, env)?, |_, expr| check_impl(expr, env)),
    }
}

pub fn check(expr: &Expr) -> Result<Type, Error> {
    let mut env = vec![];
    check_impl(expr, &mut env)
}
