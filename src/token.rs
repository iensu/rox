use std::str::Chars;

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
    Number(f64),
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
    column_offset: usize,
    source: &'a str,
    chars: itertools::PeekNth<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
            column_offset: 0,
            source,
            chars: itertools::peek_nth(source.chars()),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;

            let token = self.scan_token()?;

            if token.is_none() {
                continue;
            }

            tokens.push(token.unwrap());
        }

        tokens.push(Token::new(
            TokenType::EOF,
            "".into(),
            Literal::Null,
            self.line,
            self.current - self.column_offset,
        ));

        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.chars.next()
    }

    fn scan_token(&mut self) -> Result<Option<Token>> {
        use TokenType::*;
        let c = self.advance();

        if c.is_none() {
            return Ok(None);
        }

        let c = c.unwrap();

        match c {
            '(' => Ok(Some(self.create_token(LEFT_PAREN))),
            ')' => Ok(Some(self.create_token(RIGHT_PAREN))),
            '{' => Ok(Some(self.create_token(LEFT_BRACE))),
            '}' => Ok(Some(self.create_token(RIGHT_BRACE))),
            ',' => Ok(Some(self.create_token(COMMA))),
            '.' => Ok(Some(self.create_token(DOT))),
            '-' => Ok(Some(self.create_token(MINUS))),
            '+' => Ok(Some(self.create_token(PLUS))),
            ';' => Ok(Some(self.create_token(SEMICOLON))),
            '*' => Ok(Some(self.create_token(STAR))),
            '!' => Ok(Some(self.one_or_two_char_token(&'=', BANG, BANG_EQUAL))),
            '>' => Ok(Some(self.one_or_two_char_token(
                &'=',
                GREATER,
                GREATER_EQUAL,
            ))),
            '<' => Ok(Some(self.one_or_two_char_token(&'=', LESS, LESS_EQUAL))),
            '=' => Ok(Some(self.one_or_two_char_token(&'=', EQUAL, EQUAL_EQUAL))),
            // Whitespace
            ' ' | '\t' | '\r' => Ok(None),
            // New line
            '\n' => {
                self.line += 1;
                self.column_offset = self.current;
                Ok(None)
            }
            // Comment or SLASH
            '/' => {
                if self.chars.peek() == Some(&'/') {
                    self.advance(); // Consume the forward slash
                    while !self.is_at_end() && self.chars.peek() != Some(&'\n') {
                        self.advance();
                    }
                    Ok(None)
                } else {
                    Ok(Some(self.create_token(SLASH)))
                }
            }

            c if c.is_ascii_digit() => {
                while !self.is_at_end()
                    && (self.chars.peek().map_or(false, |c| c.is_ascii_digit())
                        || self.chars.peek() == Some(&'.')
                            && self.chars.peek_nth(1).map_or(false, |c| c.is_ascii_digit()))
                {
                    self.advance();
                }
                let lexeme = &self.source[self.start..self.current];
                let number = lexeme.parse::<f64>().unwrap();

                Ok(Some(Token::new(
                    NUMBER,
                    lexeme.into(),
                    Literal::Number(number),
                    self.line,
                    self.start - self.column_offset,
                )))
            }

            _ => {
                let line = self.line;
                let col = self.start - self.column_offset;
                return Err(eyre!("Line {line} Col {col}: Unexpected character '{c}'"));
            }
        }
    }

    fn create_token(&mut self, t: TokenType) -> Token {
        let lexeme = &self.source[self.start..self.current];

        Token::new(
            t,
            String::from_utf8(lexeme.into()).unwrap(),
            Literal::Null,
            self.line,
            self.start - self.column_offset,
        )
    }

    fn one_or_two_char_token(
        &mut self,
        check: &char,
        single: TokenType,
        double: TokenType,
    ) -> Token {
        if self.chars.peek() == Some(check) {
            self.advance();
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
            ("// a comment", vec![(12, 12, EOF)]),
            ("* // a comment", vec![(0, 1, STAR), (14, 14, EOF)]),
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

    #[test]
    fn newlines_are_handled_correctly() {
        let source = "
* () {
  + // a comment
};
"
        .trim();
        let expected = vec![
            Token::new(STAR, "*".into(), L::Null, 1, 0),
            Token::new(LEFT_PAREN, "(".into(), L::Null, 1, 2),
            Token::new(RIGHT_PAREN, ")".into(), L::Null, 1, 3),
            Token::new(LEFT_BRACE, "{".into(), L::Null, 1, 5),
            Token::new(PLUS, "+".into(), L::Null, 2, 2),
            Token::new(RIGHT_BRACE, "}".into(), L::Null, 3, 0),
            Token::new(SEMICOLON, ";".into(), L::Null, 3, 1),
            Token::new(EOF, "".into(), L::Null, 3, 2),
        ];

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn numbers_are_parsed_correctly() {
        let test_cases = vec![
            (
                "1",
                vec![
                    Token::new(NUMBER, "1".into(), L::Number(1.), 1, 0),
                    Token::new(EOF, "".into(), L::Null, 1, 1),
                ],
            ),
            (
                "42",
                vec![
                    Token::new(NUMBER, "42".into(), L::Number(42.), 1, 0),
                    Token::new(EOF, "".into(), L::Null, 1, 2),
                ],
            ),
            (
                "3.14",
                vec![
                    Token::new(NUMBER, "3.14".into(), L::Number(3.14), 1, 0),
                    Token::new(EOF, "".into(), L::Null, 1, 4),
                ],
            ),
        ];

        for (source, expected) in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn mathematical_expression() {
        let test_cases = vec![(
            "1000 + 1000000 = 1001000",
            vec![
                Token::new(NUMBER, "1000".into(), L::Number(1000.), 1, 0),
                Token::new(PLUS, "+".into(), L::Null, 1, 5),
                Token::new(NUMBER, "1000000".into(), L::Number(1000000.), 1, 7),
                Token::new(EQUAL, "=".into(), L::Null, 1, 15),
                Token::new(NUMBER, "1001000".into(), L::Number(1001000.), 1, 17),
                Token::new(EOF, "".into(), L::Null, 1, 24),
            ],
        )];

        for (source, expected) in test_cases {
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }
}
