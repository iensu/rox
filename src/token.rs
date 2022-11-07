crate::string_enum! {

    #[allow(non_camel_case_types, clippy::upper_case_acronyms)]
    #[derive(Debug, PartialEq, Eq, Copy, Clone)]
    TokenType {
        AND = "and",
        BANG = "!",
        BANG_EQUAL = "!=",
        CARET = "^",
        CLASS = "class",
        COMMA = ",",
        DOT = ".",
        ELSE = "else",
        FALSE = "false",
        FOR = "for",
        FUN = "fun",
        IF = "if",
        NIL = "nil",
        OR = "or",
        PRINT = "print",
        RETURN = "return",
        SUPER = "super",
        THIS = "this",
        TRUE = "true",
        VAR = "var",
        WHILE = "while",
        EOF = "",
        EQUAL = "=",
        EQUAL_EQUAL = "==",
        GREATER = ">",
        GREATER_EQUAL = ">=",
        LESS = "<",
        LESS_EQUAL = "<=",
        LEFT_BRACE = "{",
        RIGHT_BRACE = "}",
        LEFT_PAREN = "(",
        RIGHT_PAREN = ")",
        MINUS = "-",
        PLUS = "+",
        SEMICOLON = ";",
        SLASH = "/",
        STAR = "*",

        IDENTIFIER,
        NUMBER,
        STRING
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
