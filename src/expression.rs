use crate::token::{self, TokenType::*, Value};

use eyre::{eyre, Result};

#[derive(PartialEq, Debug)]
pub enum Expr<'a> {
    Literal(&'a token::Value),
    Unary(&'a token::Token, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, &'a token::Token, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
}

impl<'a> Expr<'a> {
    pub fn evaluate(&self) -> Result<Value> {
        match self {
            // FIXME: Refactor Value to avoid cloning, or make the type clonable in a cheap way
            Expr::Literal(v) => match v {
                Value::Null => Ok(Value::Null),
                Value::Bool(b) => Ok(Value::Bool(*b)),
                Value::Number(n) => Ok(Value::Number(*n)),
                Value::String(s) => Ok(Value::String(s.clone())),
                Value::Identifier(s) => Ok(Value::String(s.clone())),
                Value::Keyword(kw) => Ok(Value::Keyword(*kw)),
            },

            Expr::Grouping(g) => g.evaluate(),

            Expr::Unary(t, expr) if t.token_type == MINUS => match expr.evaluate()? {
                Value::Number(n) => Ok(Value::Number(n * -1.)),
                _ => Err(eyre!("The minus operator cannot be applied to {expr}")),
            },
            Expr::Unary(t, expr) if t.token_type == BANG => {
                let value = !self.is_truthy(&expr.evaluate()?);
                Ok(Value::Bool(value))
            }
            Expr::Unary(t, _) => Err(eyre!("Invalid unary token type: {t:?}")),

            Expr::Binary(left, op, right)
                if [MINUS, STAR, SLASH, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]
                    .contains(&op.token_type) =>
            {
                let left = left.evaluate()?;
                let right = right.evaluate()?;
                let (x, y) = if let (Value::Number(l), Value::Number(r)) = (left, right) {
                    (l, r)
                } else {
                    return Err(eyre!("{} can only be applied to numbers", op.lexeme));
                };

                match op.token_type {
                    STAR => Ok(Value::Number(x * y)),
                    MINUS => Ok(Value::Number(x - y)),
                    SLASH => Ok(Value::Number(x / y)),
                    GREATER => Ok(Value::Bool(x > y)),
                    GREATER_EQUAL => Ok(Value::Bool(x >= y)),
                    LESS => Ok(Value::Bool(x < y)),
                    LESS_EQUAL => Ok(Value::Bool(x <= y)),
                    _ => unreachable!(),
                }
            }
            Expr::Binary(left, op, right) if [BANG_EQUAL, EQUAL_EQUAL].contains(&op.token_type) => {
                let left = left.evaluate()?;
                let right = right.evaluate()?;
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
                let left = left.evaluate()?;
                let right = right.evaluate()?;
                match (left, right) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                    (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{l}{r}"))),
                    (Value::Number(_), _) => {
                        Err(eyre!("Right-hand side must also evaluate to a number"))
                    }
                    (Value::String(_), _) => {
                        Err(eyre!("Right-hand  side must also evaluate to a string"))
                    }
                    _ => Err(eyre!(
                        "{} can only be applied to numbers or strings",
                        op.lexeme
                    )),
                }
            }
            Expr::Binary(_, op, _) => Err(eyre!("Invalid binary token type: {op:?}")),
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Null => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }
}

impl<'a> std::fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use token::Value::*;
        match self {
            Expr::Literal(literal) => match literal {
                Null => write!(f, "null"),
                Bool(b) => write!(f, "{b}"),
                Number(n) => write!(f, "{n}"),
                String(s) => write!(f, "\"{s}\""),
                Identifier(i) => write!(f, "{i}"),
                Keyword(s) => write!(f, "{s}"),
            },
            Expr::Unary(t, e) => write!(f, "{}{}", t.lexeme, e),
            Expr::Binary(l, op, r) => write!(f, "({} {} {})", op.lexeme, l, r),
            Expr::Grouping(e) => write!(f, "(group {})", e),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Expr::*;
    use super::*;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::token::Token;
    use crate::token::TokenType::{self, *};
    use crate::token::Value as V;

    use test_log::test;

    fn t(tt: TokenType, lexeme: &str) -> Token {
        Token::new(tt, lexeme.into(), V::Null, 0, 0)
    }

    #[test]
    fn displays_ok() {
        let forty_two = V::Number(42.);
        let star = t(STAR, "*");
        let minus = t(MINUS, "-");
        let ten = V::Number(10.);
        let plus = t(PLUS, "+");
        let hundred = V::Number(100.);
        let slash = t(SLASH, "/");
        let twelve = V::Number(12.);

        let expr: Expr = Binary(
            Box::new(Binary(
                Box::new(Literal(&forty_two)),
                &star,
                Box::new(Unary(&minus, Box::new(Literal(&ten)))),
            )),
            &plus,
            Box::new(Binary(
                Box::new(Literal(&hundred)),
                &slash,
                Box::new(Literal(&twelve)),
            )),
        );

        assert_eq!(format!("{}", expr), "(+ (* 42 -10) (/ 100 12))");
        assert_eq!(
            format!("{}", Literal(&V::String("Hello".into()))),
            "\"Hello\""
        );
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
            let result = Expr::Literal(&v).evaluate().unwrap();
            assert_eq!(result, v);
        }
    }

    #[test]
    fn evaluates_grouping_expressions() {
        let expressions = [Expr::Literal(&V::Number(3.14))];

        for e in expressions {
            let expected = e.evaluate().unwrap();
            let result = Expr::Grouping(Box::new(e)).evaluate().unwrap();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn evaluates_unary_minus_expressions() {
        let minus_token = Token::new(MINUS, "-".into(), V::Null, 1, 0);
        let fortytwo = V::Number(42.);

        let expression = Expr::Unary(&minus_token, Box::new(Expr::Literal(&fortytwo)));
        let result = expression.evaluate().unwrap();

        assert_eq!(result, V::Number(-42.));

        let expression = Expr::Unary(
            &minus_token,
            Box::new(Expr::Grouping(Box::new(Expr::Unary(
                &minus_token,
                Box::new(Expr::Literal(&fortytwo)),
            )))),
        );
        let result = expression.evaluate().unwrap();

        assert_eq!(result, V::Number(42.));
    }

    #[test]
    fn minus_unary_cannot_be_applied_to_non_numbers() {
        let test_cases = ["-false", "-\"foo\"", "-null", "-true", "-foo"];

        for source in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let expression = parser.parse().unwrap();

            match expression.evaluate() {
                Err(e) => assert!(format!("{}", e).contains("minus operator cannot be applied")),
                Ok(_) => assert!(false, "Cannot apply minus to {source}"),
            }
        }
    }

    #[test]
    fn evaluates_unary_negation_expressions() {
        let bang_token = Token::new(BANG, "!".into(), V::Null, 1, 0);
        let truth = V::Bool(true);

        let expression = Expr::Unary(&bang_token, Box::new(Expr::Literal(&truth)));
        let result = expression.evaluate().unwrap();

        assert_eq!(result, V::Bool(false));

        let expression = Expr::Unary(
            &bang_token,
            Box::new(Expr::Grouping(Box::new(Expr::Unary(
                &bang_token,
                Box::new(Expr::Literal(&truth)),
            )))),
        );
        let result = expression.evaluate().unwrap();

        assert_eq!(result, V::Bool(true));
    }

    #[test]
    fn bang_unary_only_treats_nil_and_false_as_falsy() {
        let negative_cases = ["!\"\"", "!0", "!1", "!\"foo\""];

        for source in negative_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let expression = parser.parse().unwrap();
            let value = expression.evaluate().unwrap();

            assert_eq!(value, V::Bool(false));
        }

        let positive_cases = vec!["!false", "!nil"];

        for source in positive_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let expression = parser.parse().unwrap();
            let value = expression.evaluate().unwrap();

            assert_eq!(value, V::Bool(true));
        }
    }

    #[test]
    fn evaluates_arithmatic_expressions() {
        let test_cases = [
            ("1 + 1", 2.),
            ("1 - 1", 0.),
            ("2 * 2", 4.),
            ("2 / 2", 1.),
            ("2 + 4 * 10", 42.),
            ("(2 + 4) * 10", 60.),
        ];

        for (source, expected) in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let expression = parser.parse().unwrap();
            let value = expression.evaluate().unwrap();

            assert_eq!(value, V::Number(expected), "Failed to evaluate {source}");
        }
    }

    #[test]
    fn plus_concatenates_strings() {
        let source = "\"foo\" + \"bar\"";

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let expression = parser.parse().unwrap();
        let value = expression.evaluate().unwrap();

        assert_eq!(format!("{value}"), "foobar", "Failed to evaluate {source}");
    }

    #[test]
    fn evaluates_comparison_expressions() {
        let test_cases = [
            ("2 > 1", true),
            ("1 < 2", true),
            ("2 >= 2", true),
            ("2 <= 2", true),
            ("2 > 3", false),
            ("2 >= 3", false),
            ("3 <= 2", false),
            ("3 < 2", false),
        ];

        for (source, expected) in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let expression = parser.parse().unwrap();
            let value = expression.evaluate().unwrap();

            assert_eq!(value, V::Bool(expected), "Failed to evaluate {source}");
        }
    }

    #[test]
    fn evaluates_equality_expressions() {
        let test_cases = [
            ("1 == 1", true),
            ("nil == nil", true),
            ("true == true", true),
            ("\"foo\" == \"foo\"", true),
            ("1 == 2", false),
            ("nil == true", false),
            ("true == false", false),
            ("\"foo\" == \"bar\"", false),
            ("1 != 1", false),
            ("nil != nil", false),
            ("true != true", false),
            ("\"foo\" != \"foo\"", false),
            ("1 != 2", true),
            ("nil != true", true),
            ("true != false", true),
            ("\"foo\" != \"bar\"", true),
        ];

        for (source, expected) in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            let parser = Parser::new(&tokens);
            let expression = parser.parse().unwrap();
            let value = expression.evaluate().unwrap();

            assert_eq!(value, V::Bool(expected), "Failed to evaluate {source}");
        }
    }
}
