#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    // Single character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    CARET,
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

    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,
}

impl TokenType {
    pub fn as_str(&self) -> Option<&'static str> {
        match self {
            Self::AND => Some(tokens::AND),
            Self::BANG => Some(tokens::BANG),
            Self::BANG_EQUAL => Some(tokens::BANG_EQUAL),
            Self::CARET => Some(tokens::CARET),
            Self::CLASS => Some(tokens::CLASS),
            Self::COMMA => Some(tokens::COMMA),
            Self::DOT => Some(tokens::DOT),
            Self::ELSE => Some(tokens::ELSE),
            Self::EOF => Some(tokens::EOF),
            Self::EQUAL => Some(tokens::EQUAL),
            Self::EQUAL_EQUAL => Some(tokens::EQUAL_EQUAL),
            Self::FALSE => Some(tokens::FALSE),
            Self::FOR => Some(tokens::FOR),
            Self::FUN => Some(tokens::FUN),
            Self::GREATER => Some(tokens::GREATER),
            Self::GREATER_EQUAL => Some(tokens::GREATER_EQUAL),
            Self::IF => Some(tokens::IF),
            Self::LEFT_BRACE => Some(tokens::LEFT_BRACE),
            Self::LEFT_PAREN => Some(tokens::LEFT_PAREN),
            Self::LESS => Some(tokens::LESS),
            Self::LESS_EQUAL => Some(tokens::LESS_EQUAL),
            Self::MINUS => Some(tokens::MINUS),
            Self::NIL => Some(tokens::NIL),
            Self::OR => Some(tokens::OR),
            Self::PLUS => Some(tokens::PLUS),
            Self::PRINT => Some(tokens::PRINT),
            Self::RETURN => Some(tokens::RETURN),
            Self::RIGHT_BRACE => Some(tokens::RIGHT_BRACE),
            Self::RIGHT_PAREN => Some(tokens::RIGHT_PAREN),
            Self::SEMICOLON => Some(tokens::SEMICOLON),
            Self::SLASH => Some(tokens::SLASH),
            Self::STAR => Some(tokens::STAR),
            Self::SUPER => Some(tokens::SUPER),
            Self::THIS => Some(tokens::THIS),
            Self::TRUE => Some(tokens::TRUE),
            Self::VAR => Some(tokens::VAR),
            Self::WHILE => Some(tokens::WHILE),
            _ => None,
        }
    }

    pub fn from_str(x: &str) -> Option<TokenType> {
        match x {
            tokens::AND => Some(Self::AND),
            tokens::BANG => Some(Self::BANG),
            tokens::BANG_EQUAL => Some(Self::BANG_EQUAL),
            tokens::CARET => Some(Self::CARET),
            tokens::CLASS => Some(Self::CLASS),
            tokens::COMMA => Some(Self::COMMA),
            tokens::DOT => Some(Self::DOT),
            tokens::ELSE => Some(Self::ELSE),
            tokens::EOF => Some(Self::EOF),
            tokens::EQUAL => Some(Self::EQUAL),
            tokens::EQUAL_EQUAL => Some(Self::EQUAL_EQUAL),
            tokens::FALSE => Some(Self::FALSE),
            tokens::FOR => Some(Self::FOR),
            tokens::FUN => Some(Self::FUN),
            tokens::GREATER => Some(Self::GREATER),
            tokens::GREATER_EQUAL => Some(Self::GREATER_EQUAL),
            tokens::IF => Some(Self::IF),
            tokens::LEFT_BRACE => Some(Self::LEFT_BRACE),
            tokens::LEFT_PAREN => Some(Self::LEFT_PAREN),
            tokens::LESS => Some(Self::LESS),
            tokens::LESS_EQUAL => Some(Self::LESS_EQUAL),
            tokens::MINUS => Some(Self::MINUS),
            tokens::NIL => Some(Self::NIL),
            tokens::OR => Some(Self::OR),
            tokens::PLUS => Some(Self::PLUS),
            tokens::PRINT => Some(Self::PRINT),
            tokens::RETURN => Some(Self::RETURN),
            tokens::RIGHT_BRACE => Some(Self::RIGHT_BRACE),
            tokens::RIGHT_PAREN => Some(Self::RIGHT_PAREN),
            tokens::SEMICOLON => Some(Self::SEMICOLON),
            tokens::SLASH => Some(Self::SLASH),
            tokens::STAR => Some(Self::STAR),
            tokens::SUPER => Some(Self::SUPER),
            tokens::THIS => Some(Self::THIS),
            tokens::TRUE => Some(Self::TRUE),
            tokens::VAR => Some(Self::VAR),
            tokens::WHILE => Some(Self::WHILE),
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
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Identifier(s) => write!(f, "{s}"),
            Value::Keyword(kw) => write!(f, "{kw}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub literal: Value,
    pub line: usize,
    pub column: usize,
}

impl<'a> Token<'a> {
    pub fn new(
        token_type: TokenType,
        lexeme: &'a str,
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

pub mod tokens {
    pub const AND: &str = "and";
    pub const BANG: &str = "!";
    pub const BANG_EQUAL: &str = "!=";
    pub const CARET: &str = "^";
    pub const CLASS: &str = "class";
    pub const COMMA: &str = ",";
    pub const DOT: &str = ".";
    pub const ELSE: &str = "else";
    pub const FALSE: &str = "false";
    pub const FOR: &str = "for";
    pub const FUN: &str = "fun";
    pub const IF: &str = "if";
    pub const NIL: &str = "nil";
    pub const OR: &str = "or";
    pub const PRINT: &str = "print";
    pub const RETURN: &str = "return";
    pub const SUPER: &str = "super";
    pub const THIS: &str = "this";
    pub const TRUE: &str = "true";
    pub const VAR: &str = "var";
    pub const WHILE: &str = "while";
    pub const EOF: &str = "";
    pub const EQUAL: &str = "=";
    pub const EQUAL_EQUAL: &str = "==";
    pub const GREATER: &str = ">";
    pub const GREATER_EQUAL: &str = ">=";
    pub const LESS: &str = "<";
    pub const LESS_EQUAL: &str = "<=";
    pub const LEFT_BRACE: &str = "{";
    pub const RIGHT_BRACE: &str = "}";
    pub const LEFT_PAREN: &str = "(";
    pub const RIGHT_PAREN: &str = ")";
    pub const MINUS: &str = "-";
    pub const PLUS: &str = "+";
    pub const SEMICOLON: &str = ";";
    pub const SLASH: &str = "/";
    pub const STAR: &str = "*";
}
