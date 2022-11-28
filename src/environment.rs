use std::{cell::RefCell, collections::HashMap};

use crate::{
    error::RuntimeError,
    token::{Token, Value},
};

use anyhow::Result;

pub struct Environment<'a> {
    enclosing: Option<&'a Environment<'a>>,
    values: RefCell<HashMap<String, Value>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn new_inside(enclosing: &'a Environment) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn define(&self, name: String, value: &Value) {
        let _ = self.values.borrow_mut().insert(name, value.clone());
    }

    pub fn assign(&self, name: &Token, value: &Value) -> Result<()> {
        let mut values = self.values.borrow_mut();
        if values.contains_key(name.lexeme) {
            values.insert(name.lexeme.to_string(), value.clone());
        } else {
            match &self.enclosing {
                Some(env) => env.assign(name, value)?,
                None => {
                    return Err(RuntimeError::UndefinedVariable {
                        pos: (name.line, name.column),
                        name: name.lexeme.to_string(),
                    }
                    .into());
                }
            }
        }
        Ok(())
    }

    pub fn get(&self, name: &Token) -> Result<Value> {
        match self.values.borrow().get(name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(env) => env.get(name),
                None => Err(RuntimeError::UndefinedVariable {
                    pos: (name.line, name.column),
                    name: name.lexeme.to_string(),
                }
                .into()),
            },
        }
    }
}

impl<'a> std::fmt::Display for Environment<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let definitions: Vec<String> = self
            .values
            .borrow()
            .iter()
            .map(|(k, v)| format!("{k} => {v}"))
            .collect();
        write!(f, "{}", definitions.join("\n"))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::TokenType::*;

    #[test]
    fn can_define_a_variable() {
        let token = Token::new(IDENTIFIER, "x", Value::Null, 1, 1);
        let env = Environment::new();

        env.define("x".into(), &Value::Null);

        assert_eq!(env.get(&token).unwrap(), Value::Null);
    }

    #[test]
    fn can_hold_the_value_of_a_variable() {
        let token = Token::new(IDENTIFIER, "x", Value::String("foo".into()), 1, 1);
        let env = Environment::new();

        env.define("x".into(), &token.literal);

        assert_eq!(env.get(&token).unwrap(), token.literal);
    }

    #[test]
    fn getting_an_undefined_variable_results_in_error() {
        let token = Token::new(IDENTIFIER, "x", Value::Null, 1, 1);
        let env = Environment::new();

        if let Err(e) = env.get(&token) {
            let e = e.downcast::<RuntimeError>().expect("Is runtime error!");
            match e {
                RuntimeError::UndefinedVariable { name: _, pos: _ } => assert!(true),
                _ => assert!(false, "Got unexpected error {e}"),
            }
        } else {
            assert!(false, "Should fail!")
        }
    }

    #[test]
    fn assigning_an_undefined_variable_is_an_error() {
        let token = Token::new(IDENTIFIER, "x", Value::Number(42.), 1, 1);
        let env = Environment::new();

        match env
            .assign(&token, &token.literal)
            .expect_err("should fail")
            .downcast::<RuntimeError>()
            .unwrap()
        {
            RuntimeError::UndefinedVariable { .. } => assert!(true),
            e => assert!(false, "Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn assigning_a_defined_variable() {
        let token = Token::new(IDENTIFIER, "x", Value::Number(42.), 1, 1);
        let env = Environment::new();

        env.define(token.lexeme.to_string(), &token.literal);
        env.assign(&token, &Value::Number(21.)).unwrap();
        let value = env.get(&token).expect("Variable is defined");
        assert_eq!(value, Value::Number(21.));
    }

    #[test]
    fn nested_environment_lookup() {
        let x = Token::new(IDENTIFIER, "x", Value::Number(42.0), 1, 1);
        let y = Token::new(IDENTIFIER, "y", Value::Number(21.0), 1, 1);
        let z = Token::new(IDENTIFIER, "z", Value::Number(10.5), 1, 1);

        let outer = Environment::new();
        outer.define(x.lexeme.to_string(), &x.literal);
        let middle = Environment::new_inside(&outer);
        outer.define(y.lexeme.to_string(), &y.literal);
        let inner = Environment::new_inside(&middle);
        inner.define(z.lexeme.to_string(), &z.literal);

        assert_eq!(inner.get(&x).unwrap(), Value::Number(42.0));
        assert_eq!(inner.get(&y).unwrap(), Value::Number(21.0));
        assert_eq!(inner.get(&z).unwrap(), Value::Number(10.5));
    }

    #[test]
    fn nested_assignment() {
        let x = Token::new(IDENTIFIER, "x", Value::Number(42.), 1, 1);

        let outer = Environment::new();
        outer.define(x.lexeme.to_string(), &x.literal);

        let middle = Environment::new_inside(&outer);
        let inner = Environment::new_inside(&middle);
        inner.assign(&x, &Value::Number(21.)).unwrap();

        assert_eq!(inner.get(&x).unwrap(), Value::Number(21.));
    }

    #[test]
    fn shadowing() {
        let x = Token::new(IDENTIFIER, "x", Value::Null, 1, 1);

        let outer = Environment::new();
        outer.define(x.lexeme.to_string(), &Value::Number(42.));

        let inner = Environment::new_inside(&outer);
        inner.define(x.lexeme.to_string(), &Value::Number(21.));

        assert_eq!(inner.get(&x).unwrap(), Value::Number(21.));
        assert_eq!(outer.get(&x).unwrap(), Value::Number(42.));
    }
}
