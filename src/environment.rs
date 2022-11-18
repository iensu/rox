use std::{cell::RefCell, collections::HashMap};

use crate::{
    error::RuntimeError,
    token::{Token, Value},
};

use anyhow::Result;

pub struct Environment {
    values: RefCell<HashMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn define(&self, name: String, value: &Value) {
        let _ = self.values.borrow_mut().insert(name, value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<Value> {
        self.values
            .borrow()
            .get(name.lexeme)
            .ok_or_else(|| {
                RuntimeError::UndefinedVariable {
                    pos: (name.line, name.column),
                    name: name.lexeme.to_string(),
                }
                .into()
            })
            .map(|v| v.clone())
    }
}

impl std::fmt::Display for Environment {
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
}
