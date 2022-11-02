use crate::{
    expression::Expr,
    token::{
        Token,
        TokenType::{self, *},
    },
};

use eyre::{eyre, Result};
use log::trace;

pub fn parse<'a>(tokens: &'a Vec<Token>) -> Result<Expr> {
    let (expr, _) = expression(tokens, 0)?;

    Ok(expr)
}

fn expression<'a>(tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
    trace!("Started parsing expression [idx: {start_index}]");
    equality(tokens, start_index)
}

fn equality<'a>(tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
    trace!("Started parsing equality [idx: {start_index}]");
    let (mut expr, mut current) = comparison(tokens, start_index)?;
    while do_match(&[BANG_EQUAL, EQUAL_EQUAL], tokens, current + 1) {
        current += 1;
        trace!("Building equality expression [idx: {current}]");
        let operator = tokens.get(current).unwrap();
        current += 1;
        let (right, idx) = comparison(tokens, current)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        current = idx;
    }
    Ok((expr, current))
}

fn comparison<'a>(tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
    trace!("Started parsing comparison [idx: {start_index}]");
    let (mut expr, mut current) = term(tokens, start_index)?;
    while do_match(
        &[GREATER, GREATER_EQUAL, LESS, LESS_EQUAL],
        tokens,
        current + 1,
    ) {
        current += 1;
        trace!("Building comparison expression [idx: {current}]");
        let operator = tokens.get(current).unwrap();
        current += 1;
        let (right, idx) = term(tokens, current)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        current = idx;
    }
    Ok((expr, current))
}

fn term<'a>(tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
    trace!("Started parsing term [idx: {start_index}]");
    let (mut expr, mut current) = factor(tokens, start_index)?;
    while do_match(&[MINUS, PLUS], tokens, current + 1) {
        current += 1;
        trace!("Building term expression [idx: {current}]");
        let operator = tokens.get(current).unwrap();
        current += 1;
        let (right, idx) = factor(tokens, current)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        current = idx;
    }

    Ok((expr, current))
}

fn factor<'a>(tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
    trace!("Started parsing factor [idx: {start_index}]");
    let (mut expr, mut current) = unary(tokens, start_index)?;
    while do_match(&[SLASH, STAR], tokens, current + 1) {
        current += 1;
        trace!("Building factor expression [idx: {current}]");
        let operator = tokens.get(current).unwrap();
        current += 1;
        let (right, idx) = unary(tokens, current)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        current = idx;
    }

    Ok((expr, current))
}

fn unary<'a>(tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
    trace!("Started parsing unary [idx: {start_index}]");
    if do_match(&[BANG, MINUS], tokens, start_index) {
        trace!("Building unary expression [idx: {start_index}]");
        let mut current = start_index;
        let operator = tokens.get(current).unwrap();
        let (right, idx) = unary(tokens, current + 1)?;
        current = idx;
        Ok((Expr::Unary(operator, Box::new(right)), current))
    } else {
        trace!("No unary expression found [idx: {start_index}]");
        primary(tokens, start_index)
    }
}

fn primary<'a>(tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
    trace!("Started parsing primary [idx: {start_index}]");
    let token = tokens.get(start_index).unwrap();
    let mut current = start_index + 1;
    match token.token_type {
        FALSE | TRUE | NIL | STRING | NUMBER => {
            trace!(
                "  Resolved literal {:?} [idx: {start_index}]",
                token.literal
            );
            Ok((Expr::Literal(&token.literal), start_index))
        }
        LEFT_PAREN => {
            trace!("Started parsing grouping [idx: {start_index}]");
            let (expr, idx) = expression(tokens, current)?;
            trace!("Parsed sub-expression [idx: {idx}]");
            current = idx + 1;
            let token = tokens.get(current).unwrap();
            if token.token_type == RIGHT_PAREN {
                trace!("Closed sub-expression grouping [idx: {current}]");
                Ok((Expr::Grouping(Box::new(expr)), current))
            } else {
                Err(eyre!(format!(
                    "Line: {} Col: {} | Expected ')' after expression, but got {}",
                    token.line, token.column, token.lexeme
                )))
            }
        }
        _ => Err(eyre!(
            "Line: {} Col: {} | Failed to parse literal from {}",
            token.line,
            token.column,
            token.lexeme
        )),
    }
}

fn do_match(types: &[TokenType], tokens: &Vec<Token>, index: usize) -> bool {
    trace!("Matching {types:?} against token [idx: {index}]");
    tokens.get(index).map_or(false, |t| {
        types.iter().any(|token_type| t.token_type == *token_type)
    })
}

#[cfg(test)]
mod test {
    use crate::scanner::Scanner;

    use super::parse;

    #[test]
    fn parses_a_simple_expression() {
        let source = "5 + 6";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let expr = parse(&tokens).unwrap();

        assert_eq!(format!("{}", expr), "(+ 5 6)");
    }

    #[test]
    fn parses_a_more_complex_expression() {
        let source = "12 + 14 + 23 + 18";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let expr = parse(&tokens).unwrap();

        assert_eq!(format!("{}", expr), "(+ (+ (+ 12 14) 23) 18)");
    }

    #[test]
    fn parses_grouped_expressions() {
        let source = "12 * (14 + 23) / 10";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let expr = parse(&tokens).unwrap();

        assert_eq!(format!("{}", expr), "(/ (* 12 (group (+ 14 23))) 10)");
    }
}
