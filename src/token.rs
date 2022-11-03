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

impl TokenType {
    pub fn keyword(x: &str) -> Option<(TokenType, &'static str)> {
        use crate::keywords::*;

        match x {
            "and" => Some((Self::AND, AND)),
            "class" => Some((Self::CLASS, CLASS)),
            "else" => Some((Self::ELSE, ELSE)),
            "false" => Some((Self::FALSE, FALSE)),
            "fun" => Some((Self::FUN, FUN)),
            "for" => Some((Self::FOR, FOR)),
            "if" => Some((Self::IF, IF)),
            "nil" => Some((Self::NIL, NIL)),
            "or" => Some((Self::OR, OR)),
            "print" => Some((Self::PRINT, PRINT)),
            "return" => Some((Self::RETURN, RETURN)),
            "super" => Some((Self::SUPER, SUPER)),
            "this" => Some((Self::THIS, THIS)),
            "true" => Some((Self::TRUE, TRUE)),
            "var" => Some((Self::VAR, VAR)),
            "while" => Some((Self::WHILE, WHILE)),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Identifier(String),
    Keyword(&'static str),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Identifier(s) => write!(f, "{s}"),
            Value::Keyword(kw) => write!(f, "{kw}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Value,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Value,
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
