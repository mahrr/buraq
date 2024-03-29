use crate::parser::{Def, Expr, ExprKind, Prog};
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    TypeMismatch,
    AssignToConst(String),
    UnboundName(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::TypeMismatch => write!(f, "type mismatch"),
            Error::AssignToConst(name) => write!(f, "assign const name '{name}'"),
            Error::UnboundName(name) => write!(f, "unbound name '{name}'"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Boolean,
    Fn(Vec<Type>, Box<Type>),
}

pub type TypeMap = HashMap<u64, Type>; // Expression ID -> Expression Type

#[derive(Clone)]
struct NameRecord {
    name: String,
    type_: Type,
    is_const: bool,
}

fn name_record(name: &String, env: &Vec<NameRecord>) -> Result<NameRecord, Error> {
    match env.iter().rev().find(|record| &record.name == name) {
        Some(record) => Ok(record.to_owned()),
        None => Err(Error::UnboundName(name.to_owned())),
    }
}

fn check_expr(
    expr: &Expr,
    env: &mut Vec<NameRecord>,
    exprs_types: &mut TypeMap,
) -> Result<Type, Error> {
    macro_rules! T {
        ($type_:expr) => {{
            exprs_types.insert(expr.id, $type_.clone());
            Ok($type_)
        }};
    }

    macro_rules! tc_arithmetic {
        ($left:expr, $right:expr) => {{
            let left = check_expr($left, env, exprs_types)?;
            let right = check_expr($right, env, exprs_types)?;

            match (left, right) {
                (Type::I8, Type::I8) => T!(Type::I8),
                (Type::I16, Type::I16) => T!(Type::I16),
                (Type::I32, Type::I32) => T!(Type::I32),
                (Type::I64, Type::I64) => T!(Type::I64),
                (Type::F32, Type::F32) => T!(Type::F32),
                (Type::F64, Type::F64) => T!(Type::F64),
                _ => Err(Error::TypeMismatch),
            }
        }};
    }

    macro_rules! tc_comparison {
        ($left:expr, $right:expr) => {{
            let left = check_expr($left, env, exprs_types)?;
            let right = check_expr($right, env, exprs_types)?;

            match (left, right) {
                (Type::I8, Type::I8)
                | (Type::I16, Type::I16)
                | (Type::I32, Type::I32)
                | (Type::I64, Type::I64)
                | (Type::F32, Type::F32)
                | (Type::F64, Type::F64) => T!(Type::Boolean),
                _ => Err(Error::TypeMismatch),
            }
        }};
    }

    match &expr.kind {
        ExprKind::Boolean(_) => T!(Type::Boolean),
        ExprKind::Int8(_) => T!(Type::I8),
        ExprKind::Int16(_) => T!(Type::I16),
        ExprKind::Int32(_) => T!(Type::I32),
        ExprKind::Int64(_) => T!(Type::I64),
        ExprKind::Float32(_) => T!(Type::F32),
        ExprKind::Float64(_) => T!(Type::F64),
        ExprKind::Identifier(name) => T!(name_record(name, env)?.type_),
        ExprKind::Add(left, right) => tc_arithmetic!(left, right),
        ExprKind::Sub(left, right) => tc_arithmetic!(left, right),
        ExprKind::Mul(left, right) => tc_arithmetic!(left, right),
        ExprKind::Div(left, right) => tc_arithmetic!(left, right),
        ExprKind::LT(left, right) => tc_comparison!(left, right),
        ExprKind::GT(left, right) => tc_comparison!(left, right),
        ExprKind::LE(left, right) => tc_comparison!(left, right),
        ExprKind::GE(left, right) => tc_comparison!(left, right),
        ExprKind::EQ(left, right) => tc_comparison!(left, right),
        ExprKind::If(cond, then, else_) => {
            let cond = check_expr(cond, env, exprs_types)?;
            let then = check_expr(then, env, exprs_types)?;
            let else_ = check_expr(else_, env, exprs_types)?;

            if cond == Type::Boolean && then == else_ {
                T!(then)
            } else {
                Err(Error::TypeMismatch)
            }
        }
        ExprKind::Cond(clauses, last_clause) => {
            let last_clause = check_expr(last_clause, env, exprs_types)?;

            clauses
                .iter()
                .try_fold(last_clause, |last_clause, (test, form)| {
                    let test = check_expr(test, env, exprs_types)?;
                    let form = check_expr(form, env, exprs_types)?;

                    if test == Type::Boolean && form == last_clause {
                        T!(last_clause)
                    } else {
                        Err(Error::TypeMismatch)
                    }
                })
        }
        ExprKind::While(cond, body) => {
            if check_expr(cond, env, exprs_types)? == Type::Boolean {
                check_expr(body, env, exprs_types)
            } else {
                Err(Error::TypeMismatch)
            }
        }
        ExprKind::Let(bindings, body) => {
            let prev_bindings_count = env.len();

            for (name, value) in bindings {
                let name_record = NameRecord {
                    name: name.to_owned(),
                    type_: check_expr(value, env, exprs_types)?,
                    is_const: false,
                };
                env.push(name_record);
            }

            let body = check_expr(body, env, exprs_types);
            env.truncate(prev_bindings_count);
            body
        }
        ExprKind::Set(name, value) => {
            let record = name_record(name, env)?;
            let value = check_expr(value, env, exprs_types)?;

            if record.is_const {
                Err(Error::AssignToConst(name.to_owned()))
            } else if record.type_ != value {
                Err(Error::TypeMismatch)
            } else {
                T!(record.type_)
            }
        }
        ExprKind::Seq(first, rest) => rest
            .iter()
            .try_fold(check_expr(&first, env, exprs_types)?, |_, expr| {
                check_expr(expr, env, exprs_types)
            }),
        ExprKind::Lambda(parameters, return_type, body, _) => {
            let previous_env_count = env.len();
            for parameter in parameters {
                env.push(NameRecord {
                    name: parameter.0.to_owned(),
                    type_: parameter.1.to_owned(),
                    is_const: false,
                })
            }

            let body_type = check_expr(&body, env, exprs_types)?;
            env.truncate(previous_env_count);

            if body_type == *return_type {
                T!(Type::Fn(
                    parameters
                        .iter()
                        .map(|(_, t)| t.clone())
                        .collect::<Vec<Type>>(),
                    Box::new(return_type.clone()),
                ))
            } else {
                Err(Error::TypeMismatch)
            }
        }
        ExprKind::App(function, arguments) => {
            let function = check_expr(&function, env, exprs_types)?;
            let arguments = arguments.iter().try_fold(vec![], |mut arguments, expr| {
                arguments.push(check_expr(expr, env, exprs_types)?);
                Ok(arguments)
            })?;

            match function {
                Type::Fn(parameters, return_type) if arguments == parameters => T!(*return_type),
                _ => Err(Error::TypeMismatch),
            }
        }
    }
}

pub fn check(prog: &Prog) -> Result<TypeMap, Error> {
    let mut exprs_types = HashMap::new();

    // build the initial type environment from the given definitions
    let mut env = prog
        .definitions
        .iter()
        .map(|def| match def {
            Def::Fn(name, parameters, return_type, _) => {
                let parameters: Vec<Type> = parameters.iter().map(|p| p.1.to_owned()).collect();
                NameRecord {
                    name: name.to_owned(),
                    type_: Type::Fn(parameters, Box::new(return_type.to_owned())),
                    is_const: true,
                }
            }
        })
        .collect::<Vec<NameRecord>>();

    // type check the definitions
    for def in &prog.definitions {
        match def {
            Def::Fn(_, parameters, return_type, body) => {
                let previous_env_count = env.len();
                for parameter in parameters {
                    env.push(NameRecord {
                        name: parameter.0.to_owned(),
                        type_: parameter.1.to_owned(),
                        is_const: false,
                    })
                }

                let body_type = check_expr(&body, &mut env, &mut exprs_types)?;
                env.truncate(previous_env_count);

                if body_type != *return_type {
                    return Err(Error::TypeMismatch);
                }
            }
        };
    }

    // type check the main expression
    match check_expr(&prog.expression, &mut env, &mut exprs_types) {
        Ok(_) => Ok(exprs_types),
        Err(error) => Err(error),
    }
}
