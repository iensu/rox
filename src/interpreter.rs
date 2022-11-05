use crate::{
    error::RuntimeError,
    expression::{Expr, Stmt},
    token::{TokenType::*, Value},
};
use std::cell::RefCell;

use anyhow::Result;

pub struct Interpreter<'a> {
    // FIXME: Should be able to do something more generic here
    output_buffer: Option<RefCell<&'a mut String>>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            output_buffer: None,
        }
    }

    #[cfg(test)]
    pub fn with_buffer(output_buffer: &'a mut String) -> Self {
        Self {
            output_buffer: Some(RefCell::new(output_buffer)),
        }
    }

    /// Interprets a list of statements.
    pub fn interpret(&self, stmts: &[Stmt]) -> Result<()> {
        for s in stmts {
            self.execute(s)?;
        }
        Ok(())
    }

    fn execute(&self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(e) => {
                self.evaluate(e)?;
                Ok(())
            }
            Stmt::Print(e) => {
                let value = self.evaluate(e)?;
                self.println(&value.to_string());
                Ok(())
            }
        }
    }

    /// Evaluates an expression.
    pub fn evaluate(&self, expr: &Expr) -> Result<Value> {
        match expr {
            // FIXME: Refactor Value to avoid cloning, or make the type clonable in a cheap way
            Expr::Literal(v) => match v {
                Value::Null => Ok(Value::Null),
                Value::Bool(b) => Ok(Value::Bool(*b)),
                Value::Number(n) => Ok(Value::Number(*n)),
                Value::String(s) => Ok(Value::String(s.clone())),
                Value::Identifier(s) => Ok(Value::String(s.clone())),
                Value::Keyword(kw) => Ok(Value::Keyword(*kw)),
            },

            Expr::Grouping(g) => self.evaluate(g),

            Expr::Unary(t, e) if t.token_type == MINUS => match self.evaluate(e)? {
                Value::Number(n) => Ok(Value::Number(n * -1.)),
                _ => Err(RuntimeError::UnaryOperatorError {
                    op: t.lexeme.to_string(),
                    pos: (t.line, t.column),
                    right: e.to_string(),
                }
                .into()),
            },
            Expr::Unary(t, e) if t.token_type == BANG => {
                let value = !self.is_truthy(&self.evaluate(e)?);
                Ok(Value::Bool(value))
            }
            Expr::Unary(t, _) => Err(RuntimeError::UnknownOpError {
                pos: (t.line, t.column),
                op: t.lexeme.to_string(),
            }
            .into()),

            Expr::Binary(left, op, right)
                if [
                    MINUS,
                    STAR,
                    SLASH,
                    CARET,
                    GREATER,
                    GREATER_EQUAL,
                    LESS,
                    LESS_EQUAL,
                ]
                .contains(&op.token_type) =>
            {
                let (x, y) = if let (Value::Number(l), Value::Number(r)) =
                    (self.evaluate(left)?, self.evaluate(right)?)
                {
                    (l, r)
                } else {
                    return Err(RuntimeError::BinaryOperatorError {
                        pos: (op.line, op.column),
                        op: op.lexeme.to_string(),
                        left: left.to_string(),
                        right: right.to_string(),
                    }
                    .into());
                };

                match op.token_type {
                    STAR => Ok(Value::Number(x * y)),
                    MINUS => Ok(Value::Number(x - y)),
                    SLASH => {
                        if y != 0. {
                            Ok(Value::Number(x / y))
                        } else {
                            Err(RuntimeError::DivisionByZero {
                                pos: (op.line, op.column),
                            }
                            .into())
                        }
                    }
                    CARET => Ok(Value::Number(x.powf(y))),
                    GREATER => Ok(Value::Bool(x > y)),
                    GREATER_EQUAL => Ok(Value::Bool(x >= y)),
                    LESS => Ok(Value::Bool(x < y)),
                    LESS_EQUAL => Ok(Value::Bool(x <= y)),
                    _ => Err(RuntimeError::UnknownOpError {
                        pos: (op.line, op.column),
                        op: op.lexeme.to_string(),
                    }
                    .into()),
                }
            }
            Expr::Binary(left, op, right) if [BANG_EQUAL, EQUAL_EQUAL].contains(&op.token_type) => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                match op.token_type {
                    EQUAL_EQUAL => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l == r)),
                        (Value::String(l), Value::String(r)) => Ok(Value::Bool(l == r)),
                        (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l == r)),
                        (Value::Null, Value::Null) => Ok(Value::Bool(true)),
                        _ => Ok(Value::Bool(false)),
                    },
                    BANG_EQUAL => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l != r)),
                        (Value::String(l), Value::String(r)) => Ok(Value::Bool(l != r)),
                        (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),
                        (Value::Null, Value::Null) => Ok(Value::Bool(false)),
                        _ => Ok(Value::Bool(true)),
                    },
                    _ => unreachable!(),
                }
            }
            Expr::Binary(left, op, right) if op.token_type == PLUS => {
                match (self.evaluate(left)?, self.evaluate(right)?) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                    (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{l}{r}"))),
                    _ => Err(RuntimeError::BinaryOperatorError {
                        pos: (op.line, op.column),
                        op: op.lexeme.to_string(),
                        left: left.to_string(),
                        right: right.to_string(),
                    }
                    .into()),
                }
            }
            Expr::Binary(_, op, _) => Err(RuntimeError::UnknownOpError {
                pos: (op.line, op.column),
                op: op.lexeme.to_string(),
            }
            .into()),
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Null => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    fn println(&self, text: &str) {
        match &self.output_buffer {
            Some(buffer) => {
                let mut out = buffer.borrow_mut();
                out.push_str(text);
                out.push_str("\n");
            }
            None => println!("{}", text),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::token::Value as V;

    use test_log::test;

    fn interpret(stmts: &[Stmt]) -> anyhow::Result<String> {
        let mut buffer = String::new();
        let interpreter = Interpreter::with_buffer(&mut buffer);

        interpreter.interpret(stmts)?;

        let output = buffer.trim().to_string();

        Ok(output)
    }

    #[test]
    fn evaluates_literal_expressions() {
        let values = [
            V::Null,
            V::Bool(true),
            V::Number(3.14),
            V::String("foo".into()),
        ];

        for v in values {
            let stmt = Stmt::Print(Box::new(Expr::Literal(&v)));
            let output = interpret(&[stmt]).unwrap();

            assert_eq!(output, v.to_string());
        }
    }

    #[test]
    fn minus_unary_cannot_be_applied_to_non_numbers() {
        let test_cases = ["-false;", "-\"foo\";", "-null;", "-true;", "-foo;"];

        for source in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let stmts = parser.parse().unwrap();

            let err = interpret(&stmts).expect_err("{source} should fail");

            match err.downcast::<RuntimeError>().unwrap() {
                RuntimeError::UnaryOperatorError { .. } => assert!(true),
                e => assert!(false, "Cannot apply minus to {source}: {e:?}"),
            }
        }
    }

    #[test]
    fn bang_unary_only_treats_nil_and_false_as_falsy() {
        let negative_cases = ["!\"\";", "!0;", "!1;", "!\"foo\";"];

        for source in negative_cases {
            let source = format!("print {}", source);
            let mut scanner = Scanner::new(&source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let stmts = parser.parse().unwrap();
            let output = interpret(&stmts).unwrap();

            assert_eq!(output, "false");
        }

        let positive_cases = vec!["!false;", "!nil;"];

        for source in positive_cases {
            let source = format!("print {}", source);
            let mut scanner = Scanner::new(&source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let stmts = parser.parse().unwrap();
            let output = interpret(&stmts).unwrap();

            assert_eq!(output, "true");
        }
    }

    #[test]
    fn evaluates_arithmatic_expressions() {
        let test_cases = [
            ("1 + 1;", 2.),
            ("1 - 1;", 0.),
            ("2 * 2;", 4.),
            ("2 / 2;", 1.),
            ("2 ^ 2;", 4.),
            ("2 + 4 * 10;", 42.),
            ("(2 + 4) * 10;", 60.),
        ];

        for (source, expected) in test_cases {
            let source = format!("print {}", source);
            let mut scanner = Scanner::new(&source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let stmts = parser.parse().unwrap();
            let output = interpret(&stmts).unwrap();

            assert_eq!(output, format!("{}", expected));
        }
    }

    #[test]
    fn plus_concatenates_strings() {
        let source = "print \"foo\" + \"bar\";";

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let stmts = parser.parse().unwrap();
        let output = interpret(&stmts).unwrap();

        assert_eq!(output, "\"foobar\"", "Failed to evaluate {source}");
    }

    #[test]
    fn evaluates_comparison_expressions() {
        let test_cases = [
            ("2 > 1;", true),
            ("1 < 2;", true),
            ("2 >= 2;", true),
            ("2 <= 2;", true),
            ("2 > 3;", false),
            ("2 >= 3;", false),
            ("3 <= 2;", false),
            ("3 < 2;", false),
        ];

        for (source, expected) in test_cases {
            let source = format!("print {}", source);
            let mut scanner = Scanner::new(&source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let stmts = parser.parse().unwrap();
            let output = interpret(&stmts).unwrap();

            assert_eq!(
                output,
                format!("{}", expected),
                "Failed to evaluate {source}"
            );
        }
    }

    #[test]
    fn evaluates_equality_expressions() {
        let test_cases = [
            ("1 == 1;", true),
            ("nil == nil;", true),
            ("true == true;", true),
            ("\"foo\" == \"foo\";", true),
            ("1 == 2;", false),
            ("nil == true;", false),
            ("true == false;", false),
            ("\"foo\" == \"bar\";", false),
            ("1 != 1;", false),
            ("nil != nil;", false),
            ("true != true;", false),
            ("\"foo\" != \"foo\";", false),
            ("1 != 2;", true),
            ("nil != true;", true),
            ("true != false;", true),
            ("\"foo\" != \"bar\";", true),
        ];

        for (source, expected) in test_cases {
            let source = format!("print {}", source);
            let mut scanner = Scanner::new(&source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let stmts = parser.parse().unwrap();
            let output = interpret(&stmts).unwrap();

            assert_eq!(
                output,
                format!("{}", expected),
                "Failed to evaluate {source}"
            );
        }
    }
}
