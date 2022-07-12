use std::fmt;
use std::iter::Peekable;

// S-Expression Parser

#[derive(Debug, PartialEq)]
pub enum SExpr {
    Symbol(String),
    List(Vec<SExpr>),
}

#[derive(Debug, Clone, PartialEq)]
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

pub fn parse_multiple(source: &String) -> Result<Vec<SExpr>, Error> {
    let mut it = source.chars().peekable();
    let mut sexprs = vec![];

    loop {
        match parse_impl(&mut it) {
            Ok(sexpr) => sexprs.push(sexpr),
            Err(error) => return Err(error)
        }

        if it.peek() == None {
            break;
        }
    }

    Ok(sexprs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let symbol = String::from("foo");
        assert_eq!(parse(&symbol), Ok(SExpr::Symbol(symbol)));

        let symbol = String::from("foo-bar");
        assert_eq!(parse(&symbol), Ok(SExpr::Symbol(symbol)));

        let symbol = String::from("091500");
        assert_eq!(parse(&symbol), Ok(SExpr::Symbol(symbol)));

        let symbol = String::from("@+/#$%^&*?1aA_");
        assert_eq!(parse(&symbol), Ok(SExpr::Symbol(symbol)));

        let list = String::from("()");
        assert_eq!(parse(&list), Ok(SExpr::List(vec![])));

        let list = String::from("(foo bar baz)");
        assert_eq!(
            parse(&list),
            Ok(SExpr::List(vec![
                SExpr::Symbol("foo".to_string()),
                SExpr::Symbol("bar".to_string()),
                SExpr::Symbol("baz".to_string())
            ]))
        );

        let list = String::from("(())");
        assert_eq!(parse(&list), Ok(SExpr::List(vec![SExpr::List(vec![])])));

        let list = String::from("(foo bar (baz))");
        assert_eq!(
            parse(&list),
            Ok(SExpr::List(vec![
                SExpr::Symbol("foo".to_string()),
                SExpr::Symbol("bar".to_string()),
                SExpr::List(vec![SExpr::Symbol("baz".to_string())])
            ]))
        );

        let list = String::from("(foo (bar (baz) qux) foo)");
        assert_eq!(
            parse(&list),
            Ok(SExpr::List(vec![
                SExpr::Symbol("foo".to_string()),
                SExpr::List(vec![
                    SExpr::Symbol("bar".to_string()),
                    SExpr::List(vec![SExpr::Symbol("baz".to_string())]),
                    SExpr::Symbol("qux".to_string())
                ]),
                SExpr::Symbol("foo".to_string()),
            ]))
        );

        let sexprs = String::from("1 2 3");
        assert_eq!(
            parse_multiple(&sexprs),
            Ok(vec![
                SExpr::Symbol("1".to_string()),
                SExpr::Symbol("2".to_string()),
                SExpr::Symbol("3".to_string())
            ])
        );

        let sexprs = String::from("(1) (2 3) 4");
        assert_eq!(
            parse_multiple(&sexprs),
            Ok(vec![
                SExpr::List(vec![SExpr::Symbol("1".to_string())]),
                SExpr::List(vec![SExpr::Symbol("2".to_string()), SExpr::Symbol("3".to_string())]),
                SExpr::Symbol("4".to_string())
            ])
        );

        // invalid S-Expressions
        assert_eq!(parse(&String::from("")), Err(Error));
        assert_eq!(parse(&String::from(")foo bar)")), Err(Error));
        assert_eq!(parse(&String::from("(foo bar")), Err(Error));
        assert_eq!(parse(&String::from("(foo (bar (baz))")), Err(Error));
        assert_eq!(parse_multiple(&String::from("")), Err(Error));
        assert_eq!(parse_multiple(&String::from("(foo bar")), Err(Error));
        assert_eq!(parse_multiple(&String::from(")foo bar)")), Err(Error));
        assert_eq!(parse_multiple(&String::from("(foo (bar (baz))")), Err(Error));
    }
}
