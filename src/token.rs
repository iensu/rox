use std::{iter::Peekable, str::Chars};

use eyre::{eyre, Result};

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum TokenType {
    // Single character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Null,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Literal, // Here be monsters
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Literal,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
            column,
        }
    }
}

pub struct Scanner<'a> {
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    source: &'a str,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            source,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
        let mut chars = self.source.chars().peekable();
        let mut tokens: Vec<Token> = Vec::new();

        while self.current < self.source.len() {
            self.start = self.current;
            self.current += 1;

            let token = self.scan_token(&mut chars)?;
            tokens.push(token);
        }

        tokens.push(Token::new(
            TokenType::EOF,
            "".into(),
            Literal::Null,
            self.line,
            self.current,
        ));

        Ok(tokens)
    }

    fn scan_token(&mut self, chars: &mut Peekable<Chars>) -> Result<Token> {
        use TokenType::*;

        let c = chars.next().unwrap();

        match c {
            '(' => Ok(self.create_token(LEFT_PAREN)),
            ')' => Ok(self.create_token(RIGHT_PAREN)),
            '{' => Ok(self.create_token(LEFT_BRACE)),
            '}' => Ok(self.create_token(RIGHT_BRACE)),
            ',' => Ok(self.create_token(COMMA)),
            '.' => Ok(self.create_token(DOT)),
            '-' => Ok(self.create_token(MINUS)),
            '+' => Ok(self.create_token(PLUS)),
            ';' => Ok(self.create_token(SEMICOLON)),
            '*' => Ok(self.create_token(STAR)),
            '!' => {
                if let Some(_) = chars.next_if_eq(&'=') {
                    self.current += 1;
                    Ok(self.create_token(BANG_EQUAL))
                } else {
                    Ok(self.create_token(BANG))
                }
            }
            '>' => {
                if let Some(_) = chars.next_if_eq(&'=') {
                    self.current += 1;
                    Ok(self.create_token(GREATER_EQUAL))
                } else {
                    Ok(self.create_token(GREATER))
                }
            }
            '<' => {
                if let Some(_) = chars.next_if_eq(&'=') {
                    self.current += 1;
                    Ok(self.create_token(LESS_EQUAL))
                } else {
                    Ok(self.create_token(LESS))
                }
            }
            '=' => {
                if let Some(_) = chars.next_if_eq(&'=') {
                    self.current += 1;
                    Ok(self.create_token(EQUAL_EQUAL))
                } else {
                    Ok(self.create_token(EQUAL))
                }
            }
            _ => {
                return Err(eyre!(
                    "Line {} Col {}: Unexpected character '{c}'",
                    self.line,
                    self.start
                ));
            }
        }
    }

    fn create_token(&mut self, t: TokenType) -> Token {
        let lexeme = &self.source[self.start..self.current];

        Token::new(t, lexeme.into(), Literal::Null, self.line, self.start)
    }
}

#[cfg(test)]
mod test {
    use super::Literal as L;
    use super::TokenType::*;
    use super::*;

    #[test]
    fn single_tokens_are_parsed_correctly() {
        let test_cases = vec![
            ("(", LEFT_PAREN),
            (")", RIGHT_PAREN),
            ("{", LEFT_BRACE),
            ("}", RIGHT_BRACE),
            (",", COMMA),
            (".", DOT),
            ("-", MINUS),
            ("+", PLUS),
            (";", SEMICOLON),
            ("*", STAR),
            ("!", BANG),
            ("<", LESS),
            (">", GREATER),
            ("=", EQUAL),
            ("!=", BANG_EQUAL),
            ("<=", LESS_EQUAL),
            (">=", GREATER_EQUAL),
            ("==", EQUAL_EQUAL),
        ];

        for (source, token_type) in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner
                .scan_tokens()
                .expect(&format!("Failed to scan token '{source}'"));
            let expected = vec![
                Token::new(token_type, source.to_string(), L::Null, 1, 0),
                Token::new(EOF, "".into(), L::Null, 1, source.len()),
            ];

            assert_eq!(tokens, expected);
        }
    }
}
