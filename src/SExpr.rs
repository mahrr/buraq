use std::iter::Peekable;

// == S-Expression Parser ==

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

fn parse_impl<I: Iterator<Item = char>>(it: &mut Peekable<I>) -> Option<SExpr> {
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

                if let Some(expr) = parse_impl(it) {
                    elements.push(expr);
                } else {
                    // TODO: error handling
                    todo!();
                }
            }
            Some(SExpr::List(elements))
        }
        Some(_) => {
            let mut symbol: String = String::new();
            while let Some(ch) = it.next_if(|&ch| !is_delimiter(ch)) {
                symbol.push(ch);
            }

            // TODO: error handling
            if symbol.len() == 0 {
                todo!()
            }

            Some(SExpr::Symbol(symbol))
        }
        None => None,
    }
}

pub fn parse(source: &String) -> Option<SExpr> {
    parse_impl(&mut source.chars().peekable())
}