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
use log::{debug, trace, warn};

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
        trace!("parse: entering");
        let mut statements = vec![];

        while !self.match_next(&[EOF]) {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }

        Ok(statements)
    }

    fn declaration(&'a self) -> Result<Stmt<'a>> {
        trace!("declaration: entering");
        let result = if self.match_next(&[VAR]) {
            self.advance()?;
            self.var_declaration()
        } else {
            self.statement()
        };

        if let Err(e) = result {
            warn!("declaration: error = {e}");
            self.synchronize()?;
            Ok(Stmt::Null)
        } else {
            result
        }
    }

    fn var_declaration(&'a self) -> Result<Stmt<'a>> {
        trace!("var_declaration: entering");
        let token = self.consume(IDENTIFIER, "Expected variable name.")?;

        let initializer = if self.match_next(&[EQUAL]) {
            self.advance()?;
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(SEMICOLON, "Expected ';' after variable declaration.")?;

        let var_decl = Stmt::VarDecl(token, initializer);
        trace!("var_declaration: {var_decl}");

        Ok(var_decl)
    }

    fn statement(&'a self) -> Result<Stmt<'a>> {
        trace!("statement: entering");
        if self.match_next(&[PRINT]) {
            self.advance()?;
            self.print_statement()
        } else if self.match_next(&[WHILE]) {
            self.advance()?;
            self.while_statement()
        } else if self.match_next(&[LEFT_BRACE]) {
            self.advance()?;
            Ok(Stmt::Block(self.block()?))
        } else if self.match_next(&[FOR]) {
            self.advance()?;
            self.for_statement()
        } else if self.match_next(&[IF]) {
            self.advance()?;
            self.if_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&'a self) -> Result<Stmt<'a>> {
        trace!("print_statement: entering");
        let expr = self.expression()?;
        self.verify_end_of_statement()?;
        Ok(Stmt::Print(Box::new(expr)))
    }

    fn while_statement(&'a self) -> Result<Stmt<'a>> {
        self.consume(LEFT_PAREN, "Expect '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(RIGHT_PAREN, "Expect ')' after while condition")?;
        let body = self.statement()?;

        Ok(Stmt::While(Box::new(condition), Box::new(body)))
    }

    fn if_statement(&'a self) -> Result<Stmt<'a>> {
        trace!("if_statement: entering");
        self.consume(LEFT_PAREN, "Expect '(' after 'if'.")?;

        let condition = self.expression()?;
        trace!("if_statement: condition {condition}");

        self.consume(RIGHT_PAREN, "Expect ')' after if condition")?;

        let then_branch = self.statement()?;
        trace!("if_statement: then {then_branch}");

        let else_branch = if self.match_next(&[ELSE]) {
            self.advance()?;
            let stmt = self.statement()?;
            trace!("if_statement: else {stmt}");
            Some(Box::new(stmt))
        } else {
            None
        };

        let if_stmt = Stmt::If(Box::new(condition), Box::new(then_branch), else_branch);
        debug!("if_statement: {if_stmt}");
        Ok(if_stmt)
    }

    fn for_statement(&'a self) -> Result<Stmt<'a>> {
        // Implements for loops as syntactic sugar and desugars the code into variable
        // assignment, a while loop and a block with a final increment statement.

        // Parsing step
        self.consume(LEFT_PAREN, "Expect '(' after 'for'")?;

        let initializer = if self.match_next(&[SEMICOLON]) {
            self.advance()?;
            None
        } else if self.match_next(&[VAR]) {
            self.advance()?;
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.match_next(&[SEMICOLON]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(SEMICOLON, "Expect ';' after loop condition")?;

        let increment = if !self.match_next(&[RIGHT_PAREN]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(RIGHT_PAREN, "Expect ')' after loop condition")?;

        let mut body = self.statement()?;

        // Desugaring step
        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(Box::new(increment))]);
        }
        if let Some(condition) = condition {
            body = Stmt::While(Box::new(condition), Box::new(body));
        }
        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn block(&'a self) -> Result<Vec<Stmt<'a>>> {
        let mut statements = Vec::new();

        let mut end_of_block = self.match_next(&[RIGHT_BRACE, EOF]);
        while !end_of_block {
            statements.push(self.declaration()?);
            end_of_block = self.match_next(&[RIGHT_BRACE, EOF]);
        }

        self.consume(RIGHT_BRACE, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn expression_statement(&'a self) -> Result<Stmt<'a>> {
        trace!("expression_statement: entering");
        let expr = self.expression()?;
        self.verify_end_of_statement()?;
        Ok(Stmt::Expression(Box::new(expr)))
    }

    pub fn expression(&'a self) -> Result<Expr<'a>> {
        trace!("expression: entering");
        let expr = self.assignment()?;
        trace!("expression: {expr}");
        Ok(expr)
    }

    fn assignment(&'a self) -> Result<Expr<'a>> {
        trace!("assignment: entering");
        let expr = self.or()?;
        if self.match_next(&[EQUAL]) {
            let equals = self.advance()?;
            let value = self.assignment()?;

            return match expr {
                Expr::Variable(name) => {
                    let expr = Expr::Assign(name, Box::new(value));
                    debug!("assignment: {expr}");
                    Ok(expr)
                }
                _ => Err(ParseError::InvalidAssignmentTarget {
                    pos: (equals.line, equals.column),
                    lexeme: equals.lexeme.to_string(),
                }
                .into()),
            };
        }

        Ok(expr)
    }

    fn or(&'a self) -> Result<Expr<'a>> {
        trace!("or: entering");
        let mut expr = self.and()?;

        while self.match_next(&[OR]) {
            let operator = self.advance()?;
            let right = self.and()?;
            expr = Expr::Logic(Box::new(expr), operator, Box::new(right));
            debug!("or: {expr}");
        }

        Ok(expr)
    }

    fn and(&'a self) -> Result<Expr<'a>> {
        trace!("and: entering");
        let mut expr = self.equality()?;

        while self.match_next(&[AND]) {
            let operator = self.advance()?;
            let right = self.equality()?;
            expr = Expr::Logic(Box::new(expr), operator, Box::new(right));
            debug!("and: {expr}");
        }

        Ok(expr)
    }

    fn equality(&'a self) -> Result<Expr<'a>> {
        trace!("equality: entering");
        let mut expr = self.comparison()?;

        while self.match_next(&[BANG_EQUAL, EQUAL_EQUAL]) {
            trace!("equality: left {expr:?}");
            let operator = self.advance()?;
            trace!("equality: operator {operator:?}");
            let right = self.term()?;
            trace!("equality: right {right:?}");

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            debug!("equality: {expr}");
        }

        Ok(expr)
    }

    fn comparison(&'a self) -> Result<Expr<'a>> {
        trace!("comparison: entering");
        let mut expr = self.term()?;

        while self.match_next(&[GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]) {
            trace!("comparison: left {expr:?}");
            let operator = self.advance()?;
            trace!("comparison: operator {operator:?}");
            let right = self.term()?;
            trace!("comparison: right {right:?}");

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            debug!("comparison: {expr}");
        }

        Ok(expr)
    }

    fn term(&'a self) -> Result<Expr<'a>> {
        trace!("term: entering");
        let mut expr = self.factor()?;

        while self.match_next(&[PLUS, MINUS]) {
            trace!("term: left {expr:?}");
            let operator = self.advance()?;
            trace!("term: operator {operator:?}");
            let right = self.factor()?;
            trace!("term: right {right:?}");

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            debug!("term: {expr}");
        }

        Ok(expr)
    }

    fn factor(&'a self) -> Result<Expr<'a>> {
        trace!("factor: entering");
        let mut expr = self.unary()?;

        while self.match_next(&[STAR, SLASH, CARET]) {
            trace!("factor: left {expr:?}");
            let operator = self.advance()?;
            trace!("factor: operator {operator:?}");
            let right = self.unary()?;
            trace!("factor: right {right:?}");

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            debug!("factor: {expr}");
        }

        Ok(expr)
    }

    fn unary(&'a self) -> Result<Expr<'a>> {
        trace!("unary: entering");
        let expr = if self.match_next(&[BANG, MINUS]) {
            let operator = self.advance()?;
            trace!("unary: operator {operator:?}");
            let right = self.unary()?;
            trace!("unary: right {right:?}");

            let expr = Expr::Unary(operator, Box::new(right));
            debug!("unary: {expr}");
            expr
        } else {
            self.call()?
        };

        Ok(expr)
    }

    fn call(&'a self) -> Result<Expr<'a>> {
        trace!("call: entering");
        let mut expr = self.primary()?;
        trace!("call: callee {expr}");

        loop {
            if self.match_next(&[LEFT_PAREN]) {
                self.advance()?;
                expr = self.finish_call(expr)?;
                debug!("call: {expr}");
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&'a self) -> Result<Expr<'a>> {
        trace!("primary: entering");
        let token = self.advance()?;

        let expr = match token.token_type {
            FALSE | TRUE | NIL | STRING | NUMBER => Expr::Literal(&token.literal),
            IDENTIFIER => Expr::Variable(token),
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

        debug!("primary: {expr}");
        Ok(expr)
    }

    /// Synchronizes the token stream by consuming tokens until it hits a potential
    /// statement boundary from which parsing can continue.
    ///
    /// This method can be used to continue parsing after a parsing error has been
    /// encountered.
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

    /// Returns and consumes the next token if it is of type `token_type`.
    fn consume(&self, token_type: TokenType, error_message: &'static str) -> Result<&'a Token> {
        if self.match_next(&[token_type]) {
            let token = self.advance().expect("Should have a token");
            return Ok(token);
        }

        Err(anyhow::Error::msg(error_message))
    }

    fn finish_call(&'a self, callee: Expr<'a>) -> Result<Expr<'a>> {
        let mut args = Vec::new();

        if !self.match_next(&[RIGHT_PAREN]) {
            let mut has_more = true;

            while has_more {
                args.push(self.expression()?);
                has_more = self.match_next(&[COMMA]);

                if has_more {
                    self.advance()?;
                }
            }
        }

        let closing_paren = self.consume(RIGHT_PAREN, "Expect ')' after arguments.")?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            closing_paren,
            args,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::Scanner;

    use test_log::test;

    fn check(input: &str, expected: &str) {
        let scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let stmts = parser.parse().unwrap();

        assert_eq!(format!("{}", stmts.get(0).unwrap()), expected);
    }

    #[test]
    fn parses_a_simple_expression() {
        check("5 + 6;", "(+ 5 6);");
    }

    #[test]
    fn parses_a_more_complex_expression() {
        let source = "12 + 14 + 23 + 18;";
        let expected = "(+ (+ (+ 12 14) 23) 18);";

        check(source, expected);
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
            check(source, expected);
        }
    }

    #[test]
    fn parses_grouped_expressions() {
        let source = "12 * (14 + 23) / 10;";
        let expected = "(/ (* 12 (group (+ 14 23))) 10);";

        check(source, expected);
    }

    #[test]
    fn parses_unary_expressions_correctly() {
        let test_cases = vec![("-1;", "-1;"), ("!x;", "!x;")];

        for (source, expected) in test_cases {
            check(source, expected);
        }
    }

    #[test]
    fn if_statements() {
        let test_cases = vec![
            ("if (true) 42;", "if (true) 42;"),
            ("if (1 + 1 == 2) 42;", "if ((== (+ 1 1) 2)) 42;"),
            (
                r#"if (true) { print "yes"; } else { print "no"; }"#,
                r#"if (true) { print "yes"; } else { print "no"; }"#,
            ),
        ];

        for (source, expected) in test_cases {
            check(source, expected);
        }
    }

    #[test]
    fn logic_expressions() {
        let test_cases = vec![
            ("true or false;", "true or false;"),
            ("true and false;", "true and false;"),
            ("true and true or false;", "true and true or false;"),
            (
                "true and true and false or true or false;",
                "true and true and false or true or false;",
            ),
            ("false or false or true;", "false or false or true;"),
        ];

        for (source, expected) in test_cases {
            check(source, expected);
        }
    }

    #[test]
    fn call_expressions() {
        let test_cases = vec![
            ("foo();", "foo();"),
            ("foo(x);", "foo(x);"),
            ("foo(42, y, z);", "foo(42, y, z);"),
        ];

        for (source, expected) in test_cases {
            check(source, expected);
        }
    }
}
