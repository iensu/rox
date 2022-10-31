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
    chars: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            source,
            chars: source.chars().peekable(),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut got_eof = false;

        while !self.is_at_end() {
            self.start = self.current;
            let token = self.scan_token()?;
            got_eof = token.token_type == TokenType::EOF;
            tokens.push(token);

            if got_eof {
                break;
            }
        }

        if !got_eof {
            tokens.push(Token::new(
                TokenType::EOF,
                "".into(),
                Literal::Null,
                self.line,
                self.current,
            ));
        }

        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.chars.next()
    }

    fn scan_token(&mut self) -> Result<Token> {
        use TokenType::*;

        let c = self.advance();

        if c.is_none() {
            return Ok(Token::new(
                EOF,
                "".into(),
                Literal::Null,
                self.line,
                self.current,
            ));
        }
        let c = c.unwrap();

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
            '!' => Ok(self.one_or_two_char_token(&'=', BANG, BANG_EQUAL)),
            '>' => Ok(self.one_or_two_char_token(&'=', GREATER, GREATER_EQUAL)),
            '<' => Ok(self.one_or_two_char_token(&'=', LESS, LESS_EQUAL)),
            '=' => Ok(self.one_or_two_char_token(&'=', EQUAL, EQUAL_EQUAL)),
            // Whitespace
            ' ' | '\t' | '\r' => {
                self.start = self.current;
                self.scan_token()
            }
            // New line
            '\n' => {
                self.line += 1;
                self.scan_token()
            }
            // Comment or SLASH
            '/' => {
                if let Some(_) = self.chars.next_if_eq(&'/') {
                    while !self.is_at_end() && self.chars.peek() != Some(&'\n') {
                        self.advance();
                    }
                    self.scan_token()
                } else {
                    Ok(self.create_token(SLASH))
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

    fn one_or_two_char_token(
        &mut self,
        check: &char,
        single: TokenType,
        double: TokenType,
    ) -> Token {
        if let Some(_) = self.chars.next_if_eq(check) {
            self.current += 1;
            self.create_token(double)
        } else {
            self.create_token(single)
        }
    }
}

#[cfg(test)]
mod test {
    use super::Literal as L;
    use super::TokenType::*;
    use super::*;

    #[test]
    fn tokens_are_parsed_correctly() {
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
            ("/", SLASH),
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

    #[test]
    fn whitespace_is_ignored() {
        let test_cases: Vec<(&str, Vec<(usize, usize, TokenType)>)> = vec![
            (
                "( )",
                vec![(0, 1, LEFT_PAREN), (2, 3, RIGHT_PAREN), (3, 3, EOF)],
            ),
            (
                "  !=\t\t   ==",
                vec![(2, 4, BANG_EQUAL), (9, 11, EQUAL_EQUAL), (11, 11, EOF)],
            ),
        ];

        for (source, expected) in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner
                .scan_tokens()
                .expect(&format!("Failed to scan token '{source}'"));
            let expected = expected
                .iter()
                .map(|(start, end, token_type)| {
                    let lexeme = &source[*start..*end];
                    Token::new(*token_type, lexeme.into(), L::Null, 1, *start)
                })
                .collect::<Vec<Token>>();

            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn comments_are_ignored() {
        let test_cases: Vec<(&str, Vec<(usize, usize, TokenType)>)> = vec![
            ("// a comment", vec![(13, 13, EOF)]),
            ("* // a comment", vec![(0, 1, STAR), (15, 15, EOF)]),
        ];

        for (source, expected) in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner
                .scan_tokens()
                .expect(&format!("Failed to scan token '{source}'"));
            let expected = expected
                .iter()
                .map(|(start, end, token_type)| {
                    let lexeme = if *token_type == EOF {
                        ""
                    } else {
                        &source[*start..*end]
                    };
                    Token::new(*token_type, lexeme.into(), L::Null, 1, *start)
                })
                .collect::<Vec<Token>>();

            assert_eq!(tokens, expected);
        }
    }
}
