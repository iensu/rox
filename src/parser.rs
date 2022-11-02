use crate::{
    expression::Expr,
    token::{
        Token,
        TokenType::{self, *},
    },
};

use eyre::{eyre, Result};

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse<'a>(&'a self, tokens: &'a Vec<Token>) -> Result<Expr> {
        let (expr, _) = self.expression(tokens, 0)?;

        Ok(expr)
    }

    fn expression<'a>(
        &'a self,
        tokens: &'a Vec<Token>,
        start_index: usize,
    ) -> Result<(Expr, usize)> {
        println!("expression [index: {start_index}]");
        self.equality(tokens, start_index)
    }

    fn equality<'a>(&'a self, tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
        println!("equality [index: {start_index}]");
        let (mut expr, mut current) = self.comparison(tokens, start_index)?;
        while self.do_match(&[BANG_EQUAL, EQUAL_EQUAL], tokens, current + 1) {
            current += 1;
            let operator = tokens.get(current).unwrap();
            current += 1;
            let (right, idx) = self.comparison(tokens, current)?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            current = idx;
        }
        Ok((expr, current))
    }

    fn comparison<'a>(
        &'a self,
        tokens: &'a Vec<Token>,
        start_index: usize,
    ) -> Result<(Expr, usize)> {
        println!("comparison [index: {start_index}]");
        let (mut expr, mut current) = self.term(tokens, start_index)?;
        while self.do_match(
            &[GREATER, GREATER_EQUAL, LESS, LESS_EQUAL],
            tokens,
            current + 1,
        ) {
            current += 1;
            let operator = tokens.get(current).unwrap();
            current += 1;
            let (right, idx) = self.term(tokens, current)?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            current = idx;
        }
        Ok((expr, current))
    }

    fn term<'a>(&'a self, tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
        println!("term [index: {start_index}]");
        let (mut expr, mut current) = self.factor(tokens, start_index)?;
        while self.do_match(&[MINUS, PLUS], tokens, current + 1) {
            current += 1;
            let operator = tokens.get(current).unwrap();
            current += 1;
            let (right, idx) = self.factor(tokens, current)?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            current = idx;
        }

        Ok((expr, current))
    }

    fn factor<'a>(&'a self, tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
        println!("factor [index: {start_index}]");
        let (mut expr, mut current) = self.unary(tokens, start_index)?;
        while self.do_match(&[SLASH, STAR], tokens, current + 1) {
            current += 1;
            let operator = tokens.get(current).unwrap();
            current += 1;
            let (right, idx) = self.unary(tokens, current)?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            current = idx;
        }

        Ok((expr, current))
    }

    fn unary<'a>(&'a self, tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
        println!("unary [index: {start_index}]");
        if self.do_match(&[BANG, MINUS], tokens, start_index) {
            let mut current = start_index;
            let operator = tokens.get(current).unwrap();
            let (right, idx) = self.unary(tokens, current + 1)?;
            current = idx;
            Ok((Expr::Unary(operator, Box::new(right)), current))
        } else {
            self.primary(tokens, start_index)
        }
    }

    fn primary<'a>(&'a self, tokens: &'a Vec<Token>, start_index: usize) -> Result<(Expr, usize)> {
        println!("primary [index: {start_index}]");
        let token = tokens.get(start_index).unwrap();
        let mut current = start_index + 1;
        match token.token_type {
            FALSE | TRUE | NIL | STRING | NUMBER => {
                Ok((Expr::Literal(&token.literal), start_index))
            }
            LEFT_PAREN => {
                let (expr, idx) = self.expression(tokens, current)?;
                current = idx + 1;
                let token = tokens.get(current).unwrap();
                if token.token_type == RIGHT_PAREN {
                    current += 1;
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

    fn do_match(&self, types: &[TokenType], tokens: &Vec<Token>, index: usize) -> bool {
        let token = tokens.get(index);
        if token.is_none() {
            return false;
        }
        let token = token.unwrap();

        for token_type in types {
            if token.token_type == *token_type {
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod test {
    use crate::token::Scanner;

    use super::*;

    #[test]
    fn parses_a_simple_expression() {
        let source = "5 + 6";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new();
        let expr = parser.parse(&tokens).unwrap();

        assert_eq!(format!("{}", expr), "(+ 5 6)");
    }

    #[test]
    fn parses_a_more_complex_expression() {
        let source = "12 + 14 + 23 + 18";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new();
        let expr = parser.parse(&tokens).unwrap();

        assert_eq!(format!("{}", expr), "(+ (+ (+ 12 14) 23) 18)");
    }
}
