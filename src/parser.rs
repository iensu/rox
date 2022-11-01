use crate::{expression::Expr, token};

use eyre::{eyre, Result};

pub struct Parser {
    tokens: Vec<token::Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<token::Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        let expr = self.comparison()?;

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        Err(eyre!("Failed to parse comparison"))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parsing_an_empty_token_list_results_in_an_error() {
        let mut parser = Parser::new(vec![]);
        if let Err(_) = parser.parse() {
            assert!(true);
        } else {
            assert!(false, "Should fail");
        }
    }
}
