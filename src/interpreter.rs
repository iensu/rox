use crate::{
    environment::Environment,
    error::RuntimeError,
    expression::{Expr, Stmt},
    token::{TokenType::*, Value},
};
use std::cell::RefCell;

use anyhow::Result;
use log::{debug, trace, warn};

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
    pub fn interpret(&self, stmts: &[Stmt], env: &Environment) -> Result<()> {
        trace!("interpret: entering");
        for s in stmts {
            self.execute(s, env)?;
        }
        Ok(())
    }

    fn execute(&self, stmt: &Stmt, env: &Environment) -> Result<()> {
        debug!("execute: {stmt}");
        match stmt {
            Stmt::Block(stmts) => {
                let nested_env = Environment::new_inside(env);
                for stmt in stmts {
                    self.execute(stmt, &nested_env)?;
                }
                Ok(())
            }
            Stmt::Expression(e) => {
                let value = self.evaluate(e, env)?;
                debug!("execute: result = {value}");
                Ok(())
            }
            Stmt::Print(e) => {
                let value = self.evaluate(e, env)?;
                debug!("execute: result = {value}");
                self.println(&value.to_string());
                Ok(())
            }
            Stmt::VarDecl(name, initializer) => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr, env)?,
                    None => Value::Null,
                };

                debug!("execute: var {} = {}", name.lexeme, value);
                env.define(name.lexeme.to_string(), &value);
                Ok(())
            }
            Stmt::Null => {
                warn!("execute: reached null statement");
                Ok(())
            }
        }
    }

    pub fn evaluate(&self, expr: &Expr, env: &Environment) -> Result<Value> {
        match expr {
            // FIXME: Refactor Value to avoid cloning, or make the type clonable in a cheap way
            Expr::Literal(v) => Ok((*v).clone()),

            Expr::Grouping(g) => self.evaluate(g, env),

            Expr::Unary(t, e) if t.token_type == MINUS => match self.evaluate(e, env)? {
                Value::Number(n) => Ok(Value::Number(n * -1.)),
                _ => Err(RuntimeError::UnaryOperatorError {
                    op: t.lexeme.to_string(),
                    pos: (t.line, t.column),
                    right: e.to_string(),
                }
                .into()),
            },
            Expr::Unary(t, e) if t.token_type == BANG => {
                let value = !self.is_truthy(&self.evaluate(e, env)?);
                Ok(Value::Bool(value))
            }
            Expr::Unary(t, _) => Err(RuntimeError::UnknownOpError {
                pos: (t.line, t.column),
                op: t.lexeme.to_string(),
            }
            .into()),
            Expr::Assign(name, e) => {
                let value = self.evaluate(e, env)?;
                env.assign(&name, &value)?;
                Ok(Value::Void)
            }
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
                    (self.evaluate(left, env)?, self.evaluate(right, env)?)
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
                let left = self.evaluate(left, env)?;
                let right = self.evaluate(right, env)?;
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
                match (self.evaluate(left, env)?, self.evaluate(right, env)?) {
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
            Expr::Variable(value) => env.get(value),
        }
        .map(|value| {
            trace!("evaluate: result = {value}");
            value
        })
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
                out.push('\n');
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

    fn interpret_statements(stmts: &[Stmt], env: &Environment) -> anyhow::Result<String> {
        let mut buffer = String::new();
        let interpreter = Interpreter::with_buffer(&mut buffer);

        interpreter.interpret(stmts, env)?;

        let output = buffer.trim().to_string();

        Ok(output)
    }

    fn check(input: &str, expected: &str) {
        let scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let stmts = parser.parse().unwrap();
        let env = Environment::new();
        let output = interpret_statements(&stmts, &env).unwrap();

        assert_eq!(output, expected.trim())
    }

    fn check_err(input: &str) -> anyhow::Error {
        let scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let stmts = parser.parse().unwrap();
        let env = Environment::new();
        interpret_statements(&stmts, &env).expect_err("{input} should fail")
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
            let output = interpret_statements(&[stmt], &Environment::new()).unwrap();

            assert_eq!(output, v.to_string());
        }
    }

    #[test]
    fn minus_unary_cannot_be_applied_to_non_numbers() {
        let test_cases = ["-false;", "-\"foo\";", "-nil;", "-true;"];

        for source in test_cases {
            match check_err(source).downcast::<RuntimeError>().unwrap() {
                RuntimeError::UnaryOperatorError { .. } => assert!(true),
                e => assert!(false, "Unexpected error: {e:?}"),
            }
        }
    }

    #[test]
    fn bang_unary_only_treats_nil_and_false_as_falsy() {
        let negative_cases = ["!\"\";", "!0;", "!1;", "!\"foo\";"];

        for source in negative_cases {
            check(&format!("print {}", source), "false");
        }

        let positive_cases = vec!["!false;", "!nil;"];

        for source in positive_cases {
            check(&format!("print {}", source), "true");
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
            check(&format!("print {}", source), &format!("{}", expected));
        }
    }

    #[test]
    fn plus_concatenates_strings() {
        check(r#"print "foo" + "bar";"#, r#""foobar""#);
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
            check(&format!("print {}", source), &format!("{}", expected));
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
            check(&format!("print {}", source), &format!("{}", expected));
        }
    }

    #[test]
    fn variable_declaration() {
        let source = r#"
var x = 12;
var y;

print x;
print y;
"#;
        let expected = r#"
12
null
"#;
        check(source, expected);
    }

    #[test]
    fn variable_assignment() {
        let source = r#"
var x = 12;
var y;

x = 665;
y = "new value";

print x;
print y;
"#;
        let expected = r#"
665
"new value"
"#;
        check(source, expected);
    }

    #[test]
    fn error_when_assigning_to_undeclared_variable() {
        let source = r#"
foo = "undeclared variable";
"#;
        match check_err(source).downcast::<RuntimeError>().unwrap() {
            RuntimeError::UndefinedVariable { name, .. } => assert_eq!(name, "foo"),
            e => assert!(false, "Unexpected error: {e:?}"),
        }
    }

    #[test]
    fn block_scope_variable_does_not_leak() {
        let source = r#"
{
  var x = "inner scope";
}
print x;
"#;
        match check_err(source).downcast::<RuntimeError>().unwrap() {
            RuntimeError::UndefinedVariable { name, .. } => assert_eq!(name, "x"),
            e => assert!(false, "Unexpected error: {e:?}"),
        }
    }

    #[test]
    fn block_scope_shadowing() {
        let source = r#"
var foo = "outer";
{
  var foo = "inner";
  print foo;
}
print foo;
"#;
        let expected = r#"
"inner"
"outer"
"#;
        check(source, expected)
    }

    #[test]
    fn block_scope_outer_scope_access() {
        let source = r#"
var global = "outer";
{
  var local = "inner";
  print local + global;
}
"#;
        check(source, r#""innerouter""#)
    }

    #[test]
    fn nested_block_scopes() {
        let source = r#"
var a = 1;
{
  var a = 2;
  {
    var a = 3;
    {
      var a = 4;
      print a;
    }
    print a;
  }
  print a;
}
print a;
"#;
        let expected = r#"
4
3
2
1
"#;
        check(source, expected);
    }

    #[test]
    fn block_scope_assignment() {
        let source = r#"
var a = 1;

{
  a = 2;
}

print a;
"#;
        check(source, "2");
    }
}
