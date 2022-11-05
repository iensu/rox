use thiserror::Error;

use crate::token::TokenType;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("[{}:{}] The expression `{} {} {}` is invalid", .pos.0, .pos.1, .left, .op, .right)]
    BinaryOperatorError {
        pos: (usize, usize),
        op: String,
        left: String,
        right: String,
    },

    #[error("[{}:{}] The expression `{}{}` is invalid", .pos.0, .pos.1, .op, .right)]
    UnaryOperatorError {
        pos: (usize, usize),
        op: String,
        right: String,
    },

    #[error("[{}:{}] Unknown operator `{}`", .pos.0, .pos.1, .op)]
    UnknownOpError { pos: (usize, usize), op: String },

    #[error("[{}:{}] Division by zero!", .pos.0, .pos.1)]
    DivisionByZero { pos: (usize, usize) },
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unexpected end of file")]
    Eof,

    #[error("[{}:{}] Expected `)` after expression, but found `{}`", .pos.0, .pos.1, .lexeme )]
    UnbalancedParen { pos: (usize, usize), lexeme: String },

    #[error("[{}:{}] Unexpected token, expected `{}` but found `{}`", .pos.0, .pos.1, .expected, .found)]
    UnexpectedToken {
        pos: (usize, usize),
        found: String,
        expected: String,
    },

    #[error("[{}:{}] Failed to parse literal from {:?} `{}`", .pos.0, .pos.1, .token_type, .lexeme )]
    BadLiteral {
        pos: (usize, usize),
        lexeme: String,
        token_type: TokenType,
    },
}

#[derive(Error, Debug)]
pub enum ScanError {
    #[error("Unexpected end of file")]
    Eof,

    #[error("[{}:{}] Unterminated string", .pos.0, .pos.1)]
    UnterminatedString { pos: (usize, usize) },

    #[error("[{}:{}] Unexpected character `{}`", .pos.0, .pos.1, .c)]
    UnexpectedChar { pos: (usize, usize), c: char },

    #[error("[{}:{}] Failed to convert `{}` into type `{}`", .pos.0, .pos.1, .lexeme, .target_type)]
    BadConversion {
        pos: (usize, usize),
        lexeme: String,
        target_type: String,
    },
}
