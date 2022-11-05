use std::cell::{Cell, RefCell};
use std::str::Chars;

use anyhow::Result;

use crate::{
    error::ScanError,
    token::{Token, TokenType, Value},
};

type CharStream<'a> = itertools::PeekNth<Chars<'a>>;

pub struct Scanner<'a> {
    start: Cell<usize>,
    current: Cell<usize>,
    line: Cell<usize>,
    column_offset: Cell<usize>,
    source: &'a str,
    chars: RefCell<CharStream<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: Cell::new(0),
            current: Cell::new(0),
            line: Cell::new(1),
            column_offset: Cell::new(0),
            source,
            chars: RefCell::new(itertools::peek_nth(source.chars())),
        }
    }

    pub fn scan_tokens(&'a self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

        while !self.is_at_end() {
            self.start.set(self.current.get());

            if let Some(token) = self.scan_token()? {
                tokens.push(token);
            } else {
                continue;
            }
        }

        tokens.push(Token::new(
            TokenType::EOF,
            TokenType::EOF.as_str().unwrap(),
            Value::Null,
            self.line.get(),
            self.current.get() - self.column_offset.get(),
        ));

        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current.get() >= self.source.len()
    }

    fn advance(&self) -> Result<char> {
        self.current.set(self.current.get() + 1);
        self.chars
            .borrow_mut()
            .next()
            .ok_or_else(|| ScanError::Eof.into())
    }

    fn scan_token(&'a self) -> Result<Option<Token>> {
        use TokenType::*;
        let c = self.advance()?;

        match c {
            '(' => Ok(Some(self.create_token(LEFT_PAREN))),
            ')' => Ok(Some(self.create_token(RIGHT_PAREN))),
            '{' => Ok(Some(self.create_token(LEFT_BRACE))),
            '}' => Ok(Some(self.create_token(RIGHT_BRACE))),
            '^' => Ok(Some(self.create_token(CARET))),
            ',' => Ok(Some(self.create_token(COMMA))),
            '.' => Ok(Some(self.create_token(DOT))),
            '-' => Ok(Some(self.create_token(MINUS))),
            '+' => Ok(Some(self.create_token(PLUS))),
            ';' => Ok(Some(self.create_token(SEMICOLON))),
            '*' => Ok(Some(self.create_token(STAR))),
            '!' => Ok(Some(self.one_or_two_char_token(&'=', BANG, BANG_EQUAL)?)),
            '>' => Ok(Some(self.one_or_two_char_token(
                &'=',
                GREATER,
                GREATER_EQUAL,
            )?)),
            '<' => Ok(Some(self.one_or_two_char_token(&'=', LESS, LESS_EQUAL)?)),
            '=' => Ok(Some(self.one_or_two_char_token(
                &'=',
                EQUAL,
                EQUAL_EQUAL,
            )?)),
            // Whitespace
            ' ' | '\t' | '\r' => Ok(None),
            // New line
            '\n' => {
                self.newline();
                Ok(None)
            }
            // Comment or SLASH
            '/' => {
                if self.chars.borrow_mut().peek() == Some(&'/') {
                    self.advance()?; // Consume the forward slash
                    while !self.is_at_end() && self.chars.borrow_mut().peek() != Some(&'\n') {
                        self.advance()?;
                    }
                    Ok(None)
                } else {
                    Ok(Some(self.create_token(SLASH)))
                }
            }
            '"' => {
                let column = self.start.get() - self.column_offset.get();
                let start_line = self.line.get();

                loop {
                    match self.advance()? {
                        '"' => break,
                        '\n' => self.newline(),
                        _ => continue,
                    };
                }
                let lexeme = &self.source[self.start.get()..self.current.get()];
                let string = &self.source[self.start.get() + 1..self.current.get() - 1];

                Ok(Some(Token::new(
                    STRING,
                    lexeme,
                    Value::String(string.into()),
                    start_line,
                    column,
                )))
            }

            c if c.is_ascii_digit() => {
                loop {
                    if self.is_at_end() {
                        break;
                    }

                    let (next_is_digit, next_is_dot_digit) = {
                        let mut chars = self.chars.borrow_mut();
                        let is_digit = chars.peek().map_or(false, |c| c.is_ascii_digit());
                        let is_dot_digit = chars.peek() == Some(&'.')
                            && chars.peek_nth(1).map_or(false, |c| c.is_ascii_digit());

                        (is_digit, is_dot_digit)
                    };

                    if next_is_digit || next_is_dot_digit {
                        self.advance()?;
                    } else {
                        break;
                    }
                }

                let lexeme = &self.source[self.start.get()..self.current.get()];
                let number = lexeme
                    .parse::<f64>()
                    .map_err(|_| ScanError::BadConversion {
                        pos: (self.start.get(), self.current.get()),
                        lexeme: lexeme.to_string(),
                        target_type: "f64".to_string(),
                    })?;

                Ok(Some(Token::new(
                    NUMBER,
                    lexeme,
                    Value::Number(number),
                    self.line.get(),
                    self.start.get() - self.column_offset.get(),
                )))
            }

            c if c.is_ascii_alphabetic() => {
                while !self.is_at_end()
                    && self.chars.borrow_mut().peek().map_or(false, |c| {
                        c.is_ascii_alphabetic() || c.is_ascii_digit() || c == &'_'
                    })
                {
                    self.advance()?;
                }
                let lexeme = &self.source[self.start.get()..self.current.get()];

                let token_type = TokenType::from_str(lexeme).unwrap_or(IDENTIFIER);
                let literal = match token_type {
                    TRUE => Value::Bool(true),
                    FALSE => Value::Bool(false),
                    NIL => Value::Null,
                    IDENTIFIER => Value::Identifier(lexeme.into()),
                    _ => {
                        let lexeme = token_type
                            .as_str()
                            .expect("A keyword should have a str representation");
                        Value::Keyword(lexeme)
                    }
                };

                Ok(Some(Token::new(
                    token_type,
                    lexeme,
                    literal,
                    self.line.get(),
                    self.start.get() - self.column_offset.get(),
                )))
            }

            _ => {
                let line = self.line.get();
                let col = self.start.get() - self.column_offset.get();
                Err(ScanError::UnexpectedChar {
                    pos: (line, col),
                    c,
                }
                .into())
            }
        }
    }

    fn newline(&self) {
        self.line.set(self.line.get() + 1);
        self.column_offset.set(self.current.get());
    }

    fn create_token(&self, t: TokenType) -> Token {
        let lexeme = t
            .as_str()
            .unwrap_or(&self.source[self.start.get()..self.current.get()]);

        Token::new(
            t,
            lexeme,
            Value::Null,
            self.line.get(),
            self.start.get() - self.column_offset.get(),
        )
    }

    fn one_or_two_char_token(
        &self,
        check: &char,
        single: TokenType,
        double: TokenType,
    ) -> Result<Token> {
        if self.chars.borrow_mut().peek() == Some(check) {
            self.advance()?;
            Ok(self.create_token(double))
        } else {
            Ok(self.create_token(single))
        }
    }
}

#[cfg(test)]
mod test {
    use super::TokenType::*;
    use super::Value as L;
    use super::*;
    use crate::token::tokens;

    use test_log::test;

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
            let scanner = Scanner::new(source);
            let tokens = scanner
                .scan_tokens()
                .unwrap_or_else(|_| panic!("Failed to scan token '{source}'"));
            let expected = vec![
                Token::new(token_type, source, L::Null, 1, 0),
                Token::new(EOF, tokens::EOF, L::Null, 1, source.len()),
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
            let scanner = Scanner::new(source);
            let tokens = scanner
                .scan_tokens()
                .unwrap_or_else(|_| panic!("Failed to scan token '{source}'"));
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
            let scanner = Scanner::new(source);
            let tokens = scanner
                .scan_tokens()
                .unwrap_or_else(|_| panic!("Failed to scan token '{source}'"));
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

        let scanner = Scanner::new(source);
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
            let scanner = Scanner::new(source);
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
            let scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn strings_are_parsed_correctly() {
        let test_cases = vec![(
            "\"a string\"",
            vec![
                Token::new(
                    STRING,
                    "\"a string\"".into(),
                    L::String("a string".into()),
                    1,
                    0,
                ),
                Token::new(EOF, "".into(), L::Null, 1, 10),
            ],
        )];

        for (source, expected) in test_cases {
            let scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn mutliline_strings_are_parsed_correctly() {
        let test_cases = vec![(
            "\"a
multiline
string\"",
            vec![
                Token::new(
                    STRING,
                    "\"a
multiline
string\""
                        .into(),
                    L::String(
                        "a
multiline
string"
                            .into(),
                    ),
                    1,
                    0,
                ),
                Token::new(EOF, "".into(), L::Null, 3, 7),
            ],
        )];

        for (source, expected) in test_cases {
            let scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn identifiers_are_parsed_correctly() {
        let test_cases = vec![
            (
                "foo",
                vec![
                    Token::new(IDENTIFIER, "foo".into(), L::Identifier("foo".into()), 1, 0),
                    Token::new(EOF, "".into(), L::Null, 1, 3),
                ],
            ),
            (
                "another_example",
                vec![
                    Token::new(
                        IDENTIFIER,
                        "another_example".into(),
                        L::Identifier("another_example".into()),
                        1,
                        0,
                    ),
                    Token::new(EOF, "".into(), L::Null, 1, 15),
                ],
            ),
            (
                "abc123",
                vec![
                    Token::new(
                        IDENTIFIER,
                        "abc123".into(),
                        L::Identifier("abc123".into()),
                        1,
                        0,
                    ),
                    Token::new(EOF, "".into(), L::Null, 1, 6),
                ],
            ),
        ];

        for (source, expected) in test_cases {
            let scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn keywords_are_parsed_correctly() {
        let test_cases = vec![
            ("and", AND, L::Keyword(tokens::AND), 0, 3),
            ("class", CLASS, L::Keyword(tokens::CLASS), 0, 5),
            ("else", ELSE, L::Keyword(tokens::ELSE), 0, 4),
            ("false", FALSE, L::Bool(false), 0, 5),
            ("fun", FUN, L::Keyword(tokens::FUN), 0, 3),
            ("for", FOR, L::Keyword(tokens::FOR), 0, 3),
            ("if", IF, L::Keyword(tokens::IF), 0, 2),
            ("nil", NIL, L::Null, 0, 3),
            ("or", OR, L::Keyword(tokens::OR), 0, 2),
            ("print", PRINT, L::Keyword(tokens::PRINT), 0, 5),
            ("return", RETURN, L::Keyword(tokens::RETURN), 0, 6),
            ("super", SUPER, L::Keyword(tokens::SUPER), 0, 5),
            ("this", THIS, L::Keyword(tokens::THIS), 0, 4),
            ("true", TRUE, L::Bool(true), 0, 4),
            ("var", VAR, L::Keyword(tokens::VAR), 0, 3),
            ("while", WHILE, L::Keyword(tokens::WHILE), 0, 5),
        ];

        for (source, keyword, literal, start, end) in test_cases {
            let expected = vec![
                Token::new(keyword, source.into(), literal, 1, start),
                Token::new(EOF, "".into(), L::Null, 1, end),
            ];
            let scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn variable_assignment() {
        let source = "var x = 42 / (20 + 22);";

        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::new(VAR, "var".into(), L::Keyword(tokens::VAR), 1, 0),
                Token::new(IDENTIFIER, "x".into(), L::Identifier("x".into()), 1, 4),
                Token::new(EQUAL, "=".into(), L::Null, 1, 6),
                Token::new(NUMBER, "42".into(), L::Number(42.), 1, 8),
                Token::new(SLASH, "/".into(), L::Null, 1, 11),
                Token::new(LEFT_PAREN, "(".into(), L::Null, 1, 13),
                Token::new(NUMBER, "20".into(), L::Number(20.), 1, 14),
                Token::new(PLUS, "+".into(), L::Null, 1, 17),
                Token::new(NUMBER, "22".into(), L::Number(22.), 1, 19),
                Token::new(RIGHT_PAREN, ")".into(), L::Null, 1, 21),
                Token::new(SEMICOLON, ";".into(), L::Null, 1, 22),
                Token::new(EOF, "".into(), L::Null, 1, 23),
            ]
        );
    }

    #[test]
    fn function_declaration() {
        let source = "
fun square(x) {
  return x * x;
}
"
        .trim();

        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        let expected = vec![
            Token::new(FUN, "fun".into(), L::Keyword(tokens::FUN), 1, 0),
            Token::new(
                IDENTIFIER,
                "square".into(),
                L::Identifier("square".into()),
                1,
                4,
            ),
            Token::new(LEFT_PAREN, "(".into(), L::Null, 1, 10),
            Token::new(IDENTIFIER, "x".into(), L::Identifier("x".into()), 1, 11),
            Token::new(RIGHT_PAREN, ")".into(), L::Null, 1, 12),
            Token::new(LEFT_BRACE, "{".into(), L::Null, 1, 14),
            Token::new(RETURN, "return".into(), L::Keyword(tokens::RETURN), 2, 2),
            Token::new(IDENTIFIER, "x".into(), L::Identifier("x".into()), 2, 9),
            Token::new(STAR, "*".into(), L::Null, 2, 11),
            Token::new(IDENTIFIER, "x".into(), L::Identifier("x".into()), 2, 13),
            Token::new(SEMICOLON, ";".into(), L::Null, 2, 14),
            Token::new(RIGHT_BRACE, "}".into(), L::Null, 3, 0),
            Token::new(EOF, "".into(), L::Null, 3, 1),
        ];

        assert_eq!(tokens, expected);
    }
}
