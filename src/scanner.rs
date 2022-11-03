use std::str::Chars;

use eyre::{eyre, Result};

use crate::token::{Token, TokenType, Value};

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

            if let Some(token) = self.scan_token()? {
                tokens.push(token);
            } else {
                continue;
            }
        }

        tokens.push(Token::new(
            TokenType::EOF,
            "".into(),
            Value::Null,
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

        let c = c.ok_or(eyre!("Could not get character!"))?;

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
                self.newline();
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
            '"' => {
                let column = self.start - self.column_offset;
                let start_line = self.line;

                loop {
                    if let Some(c) = self.advance() {
                        if c == '"' {
                            break;
                        } else if c == '\n' {
                            self.newline();
                        }
                    } else {
                        return Err(eyre!(
                            "Line {start_line} Col {column}: Unterminated string!"
                        ));
                    }
                }
                let lexeme = &self.source[self.start..self.current];
                let string = &self.source[self.start + 1..self.current - 1];

                Ok(Some(Token::new(
                    STRING,
                    lexeme.into(),
                    Value::String(string.into()),
                    start_line,
                    column,
                )))
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
                let number = lexeme
                    .parse::<f64>()
                    .map_err(|_| eyre!("Failed to parse '{lexeme}' as f64"))?;

                Ok(Some(Token::new(
                    NUMBER,
                    lexeme.into(),
                    Value::Number(number),
                    self.line,
                    self.start - self.column_offset,
                )))
            }

            c if c.is_ascii_alphabetic() => {
                while !self.is_at_end()
                    && self.chars.peek().map_or(false, |c| {
                        c.is_ascii_alphabetic() || c.is_ascii_digit() || c == &'_'
                    })
                {
                    self.advance();
                }
                let lexeme = &self.source[self.start..self.current];

                let (token_type, literal) =
                    if let Some((keyword, string)) = TokenType::keyword(lexeme) {
                        match keyword {
                            TRUE => (keyword, Value::Bool(true)),
                            FALSE => (keyword, Value::Bool(false)),
                            NIL => (keyword, Value::Null),
                            _ => (keyword, Value::Keyword(string)),
                        }
                    } else {
                        (IDENTIFIER, Value::Identifier(lexeme.into()))
                    };

                Ok(Some(Token::new(
                    token_type,
                    lexeme.into(),
                    literal,
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

    fn newline(&mut self) {
        self.line += 1;
        self.column_offset = self.current;
    }

    fn create_token(&mut self, t: TokenType) -> Token {
        let lexeme = &self.source[self.start..self.current];

        Token::new(
            t,
            lexeme.into(),
            Value::Null,
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
    use super::TokenType::*;
    use super::Value as L;
    use super::*;
    use crate::keywords as kw;

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
            let mut scanner = Scanner::new(source);
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
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn unterminated_strings_are_handled_correctly() {
        let mut scanner = Scanner::new("\"unterminated string");
        match scanner.scan_tokens() {
            Ok(_) => assert!(false, "Should generate an error!"),
            Err(e) => assert!(
                e.to_string().contains("Unterminated string"),
                "Caused by an unterminated string"
            ),
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
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn keywords_are_parsed_correctly() {
        let test_cases = vec![
            ("and", AND, L::Keyword(kw::AND), 0, 3),
            ("class", CLASS, L::Keyword(kw::CLASS), 0, 5),
            ("else", ELSE, L::Keyword(kw::ELSE), 0, 4),
            ("false", FALSE, L::Bool(false), 0, 5),
            ("fun", FUN, L::Keyword(kw::FUN), 0, 3),
            ("for", FOR, L::Keyword(kw::FOR), 0, 3),
            ("if", IF, L::Keyword(kw::IF), 0, 2),
            ("nil", NIL, L::Null, 0, 3),
            ("or", OR, L::Keyword(kw::OR), 0, 2),
            ("print", PRINT, L::Keyword(kw::PRINT), 0, 5),
            ("return", RETURN, L::Keyword(kw::RETURN), 0, 6),
            ("super", SUPER, L::Keyword(kw::SUPER), 0, 5),
            ("this", THIS, L::Keyword(kw::THIS), 0, 4),
            ("true", TRUE, L::Bool(true), 0, 4),
            ("var", VAR, L::Keyword(kw::VAR), 0, 3),
            ("while", WHILE, L::Keyword(kw::WHILE), 0, 5),
        ];

        for (source, keyword, literal, start, end) in test_cases {
            let expected = vec![
                Token::new(keyword, source.into(), literal, 1, start),
                Token::new(EOF, "".into(), L::Null, 1, end),
            ];
            let mut scanner = Scanner::new(source);
            let tokens = scanner.scan_tokens().unwrap();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn variable_assignment() {
        let source = "var x = 42 / (20 + 22);";

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::new(VAR, "var".into(), L::Keyword(kw::VAR), 1, 0),
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

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        let expected = vec![
            Token::new(FUN, "fun".into(), L::Keyword(kw::FUN), 1, 0),
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
            Token::new(RETURN, "return".into(), L::Keyword(kw::RETURN), 2, 2),
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
