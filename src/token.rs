use std::{iter::Peekable, str::Chars};

use eyre::{eyre, Result};

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Copy, Clone)]
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Literal {
    Null,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub literal: Literal, // Here be monsters
    pub line: usize,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, lexeme: &'a str, literal: Literal, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}

pub struct Scanner<'a> {
    start: usize,
    current: usize,
    line: usize,
    source: &'a str,
    tokens: Vec<Token<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
            tokens: vec![],
            source,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
        let mut chars = self.source.chars().peekable();

        while self.current < self.source.len() {
            self.start = self.current;
            self.current += 1;

            self.scan_token(&mut chars)?;
        }

        self.tokens
            .push(Token::new(TokenType::EOF, "", Literal::Null, self.line));

        Ok(self.tokens.clone())
    }

    fn scan_token(&mut self, chars: &mut Peekable<Chars>) -> Result<()> {
        use TokenType::*;

        let c = chars.next().unwrap();

        match c {
            '(' => self.add_token(LEFT_PAREN),
            ')' => self.add_token(RIGHT_PAREN),
            '{' => self.add_token(LEFT_BRACE),
            '}' => self.add_token(RIGHT_BRACE),
            ',' => self.add_token(COMMA),
            '.' => self.add_token(DOT),
            '-' => self.add_token(MINUS),
            '+' => self.add_token(PLUS),
            ';' => self.add_token(SEMICOLON),
            '*' => self.add_token(STAR),
            '!' => {
                if let Some(_) = chars.next_if_eq(&'=') {
                    self.current += 1;
                    self.add_token(BANG_EQUAL)
                } else {
                    self.add_token(BANG)
                }
            }
            _ => {
                return Err(eyre!("Line {}: Unexpected character '{c}'", self.line));
            }
        }

        Ok(())
    }

    fn add_token(&mut self, t: TokenType) {
        let lexeme = &self.source[self.start..self.current];

        self.tokens
            .push(Token::new(t, lexeme, Literal::Null, self.line));
    }
}

#[cfg(test)]
mod test {
    use super::Literal as L;
    use super::TokenType::*;
    use super::*;

    #[test]
    fn single_tokens_are_parsed_correctly() {
        let source = "(){},.-+;*!";
        let expected = vec![
            Token::new(LEFT_PAREN, "(", L::Null, 1),
            Token::new(RIGHT_PAREN, ")", L::Null, 1),
            Token::new(LEFT_BRACE, "{", L::Null, 1),
            Token::new(RIGHT_BRACE, "}", L::Null, 1),
            Token::new(COMMA, ",", L::Null, 1),
            Token::new(DOT, ".", L::Null, 1),
            Token::new(MINUS, "-", L::Null, 1),
            Token::new(PLUS, "+", L::Null, 1),
            Token::new(SEMICOLON, ";", L::Null, 1),
            Token::new(STAR, "*", L::Null, 1),
            Token::new(BANG, "!", L::Null, 1),
            Token::new(EOF, "", L::Null, 1),
        ];

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().expect("Program can be scanned");

        assert_eq!(tokens, expected);
    }

    #[test]
    fn two_char_tokens_are_parsed_correctly() {
        let source = "!=<=>===";
        let expected = vec![
            Token::new(BANG_EQUAL, "!=", L::Null, 1),
            Token::new(LESS_EQUAL, "<=", L::Null, 1),
            Token::new(GREATER_EQUAL, ">=", L::Null, 1),
            Token::new(EQUAL_EQUAL, "==", L::Null, 1),
            Token::new(EOF, "", L::Null, 1),
        ];

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().expect("Program can be scanned");

        assert_eq!(tokens, expected);
    }
}
