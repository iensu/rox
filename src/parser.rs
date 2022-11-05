use std::{cell::RefCell, slice::Iter};

use crate::{
    error::ParseError,
    expression::{Expr, Stmt},
    token::{
        Token,
        TokenType::{self, *},
    },
};

use anyhow::Result;
use itertools::PeekNth;
use log::{debug, trace};

type TokenStream<'a> = PeekNth<Iter<'a, Token<'a>>>;

pub struct Parser<'a> {
    tokens: RefCell<TokenStream<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: RefCell::new(itertools::peek_nth(tokens.iter())),
        }
    }

    pub fn parse(&'a self) -> Result<Vec<Stmt<'a>>> {
        let mut statements = vec![];

        while !self.match_next(&[EOF]) {
            let stmt = self.print_statement()?;
            statements.push(stmt);
        }

        Ok(statements)
    }

    fn print_statement(&'a self) -> Result<Stmt<'a>> {
        if self.match_next(&[PRINT]) {
            self.advance()?;
            let expr = self.expression()?;
            self.verify_end_of_statement()?;

            Ok(Stmt::Print(Box::new(expr)))
        } else {
            self.expression_statement()
        }
    }

    fn expression_statement(&'a self) -> Result<Stmt<'a>> {
        let expr = self.expression()?;
        self.verify_end_of_statement()?;
        Ok(Stmt::Expression(Box::new(expr)))
    }

    fn expression(&'a self) -> Result<Expr<'a>> {
        let expr = self.equality()?;
        trace!("expression: {expr}");
        Ok(expr)
    }

    fn equality(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.comparison()?;

        while self.match_next(&[BANG_EQUAL, EQUAL_EQUAL]) {
            debug!("equality left: {expr:?}");
            let operator = self.advance()?;
            debug!("equality operator: {operator:?}");
            let right = self.term()?;
            debug!("equality right: {right:?}");

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        trace!("equality: {expr}");
        Ok(expr)
    }

    fn comparison(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.term()?;

        while self.match_next(&[GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]) {
            debug!("comparison left: {expr:?}");
            let operator = self.advance()?;
            debug!("comparison operator: {operator:?}");
            let right = self.term()?;
            debug!("comparison right: {right:?}");

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        trace!("comparison: {expr}");
        Ok(expr)
    }

    fn term(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.factor()?;

        while self.match_next(&[PLUS, MINUS]) {
            debug!("term left: {expr:?}");
            let operator = self.advance()?;
            debug!("term operator: {operator:?}");
            let right = self.factor()?;
            debug!("term right: {right:?}");

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        trace!("term: {expr}");
        Ok(expr)
    }

    fn factor(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.unary()?;

        while self.match_next(&[STAR, SLASH, CARET]) {
            debug!("factor left: {expr:?}");
            let operator = self.advance()?;
            debug!("factor operator: {operator:?}");
            let right = self.unary()?;
            debug!("factor right: {right:?}");

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        trace!("factor: {expr}");
        Ok(expr)
    }

    fn unary(&'a self) -> Result<Expr<'a>> {
        let expr = if self.match_next(&[BANG, MINUS]) {
            let operator = self.advance()?;
            debug!("unary operator: {operator:?}");
            let right = self.unary()?;
            debug!("unary right: {right:?}");

            Expr::Unary(operator, Box::new(right))
        } else {
            self.primary()?
        };

        trace!("unary: {expr}");
        Ok(expr)
    }

    fn primary(&'a self) -> Result<Expr<'a>> {
        let token = self.advance()?;

        let expr = match token.token_type {
            FALSE | TRUE | NIL | STRING | NUMBER | IDENTIFIER => Expr::Literal(&token.literal),
            LEFT_PAREN => {
                let expr = self.expression()?;

                let token = self.advance()?;

                if token.token_type == RIGHT_PAREN {
                    Expr::Grouping(Box::new(expr))
                } else {
                    return Err(ParseError::UnbalancedParen {
                        pos: (token.line, token.column),
                        lexeme: token.lexeme.to_string(),
                    }
                    .into());
                }
            }
            EOF => return Err(ParseError::Eof.into()),
            _ => {
                return Err(ParseError::BadLiteral {
                    pos: (token.line, token.column),
                    lexeme: token.lexeme.to_string(),
                    token_type: token.token_type,
                }
                .into());
            }
        };

        trace!("primary: {expr}");
        Ok(expr)
    }

    /// Synchronizes the token stream by consuming tokens until it hits a potential
    /// statement boundary from which parsing can continue.
    ///
    /// This method can be used to continue parsing after a parsing error has been
    /// encountered.
    #[allow(dead_code)]
    fn synchronize(&'a self) -> Result<()> {
        let mut previous = self.advance()?;

        while !self.match_next(&[EOF]) {
            if previous.token_type == SEMICOLON {
                return Ok(());
            }
            if self.match_next(&[CLASS, FOR, FUN, IF, PRINT, RETURN, VAR, WHILE]) {
                return Ok(());
            }

            previous = self.advance()?;
        }

        // Hit the end of file
        Ok(())
    }

    /// Verifies that the next token closes the current statement. Consumes the next token.
    fn verify_end_of_statement(&'a self) -> Result<()> {
        let next = self.advance()?;
        match next.token_type {
            SEMICOLON => Ok(()),
            _ => Err(ParseError::UnexpectedToken {
                pos: (next.line, next.column),
                found: next.lexeme.to_string(),
                expected: ";".to_string(),
            }
            .into()),
        }
    }

    /// Returns the next token, consuming it from the token stream.
    fn advance(&self) -> Result<&'a Token> {
        self.tokens
            .borrow_mut()
            .next()
            .ok_or_else(|| ParseError::Eof.into())
    }

    /// Returns `true` if the type of the next token in the token stream matches one
    /// of the provided `types`. Does not consume the token it looks at.
    fn match_next(&self, types: &[TokenType]) -> bool {
        self.tokens.borrow_mut().peek_nth(0).map_or(false, |t| {
            types.iter().any(|token_type| t.token_type == *token_type)
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::Scanner;

    use test_log::test;

    #[test]
    fn parses_a_simple_expression() {
        let source = "5 + 6;";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let stmts = parser.parse().unwrap();

        assert_eq!(format!("{}", stmts.get(0).unwrap()), "(+ 5 6);");
    }

    #[test]
    fn parses_a_more_complex_expression() {
        let source = "12 + 14 + 23 + 18;";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let stmts = parser.parse().unwrap();

        assert_eq!(
            format!("{}", stmts.get(0).unwrap()),
            "(+ (+ (+ 12 14) 23) 18);"
        );
    }

    #[test]
    fn factors_and_terms_has_the_correct_presedence() {
        let test_cases = vec![
            ("1 + 2 * 3;", "(+ 1 (* 2 3));"),
            ("2 * 3 + 1;", "(+ (* 2 3) 1);"),
            ("5 - 4 / 3;", "(- 5 (/ 4 3));"),
            ("5 / 4 - 3;", "(- (/ 5 4) 3);"),
            ("5 + 4 ^ 2;", "(+ 5 (^ 4 2));"),
            ("5 + 4 * 3 - 2 / 1;", "(- (+ 5 (* 4 3)) (/ 2 1));"),
        ];

        for (source, expected) in test_cases {
            let scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let stmts = parser.parse().unwrap();

            assert_eq!(format!("{}", stmts.get(0).unwrap()), expected);
        }
    }

    #[test]
    fn parses_grouped_expressions() {
        let source = "12 * (14 + 23) / 10;";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let stmts = parser.parse().unwrap();

        assert_eq!(
            format!("{}", stmts.get(0).unwrap()),
            "(/ (* 12 (group (+ 14 23))) 10);"
        );
    }

    #[test]
    fn parses_unary_expressions_correctly() {
        let test_cases = vec![("-1;", "-1;"), ("!x;", "!x;")];

        for (source, expected) in test_cases {
            let scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let stmts = parser.parse().unwrap();

            assert_eq!(format!("{}", stmts.get(0).unwrap()), expected);
        }
    }
}
