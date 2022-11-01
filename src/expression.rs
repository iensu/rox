use crate::token;

pub enum Expr {
    Literal(Box<token::Token>),
    Unary(Box<token::Token>, Box<Expr>),
    Binary(Box<Expr>, Box<token::Token>, Box<Expr>),
    Grouping(Box<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(t) => write!(f, "{}", t.lexeme),
            Expr::Unary(t, e) => write!(f, "{}{}", t.lexeme, e),
            Expr::Binary(l, op, r) => write!(f, "({} {} {})", op.lexeme, l, r),
            Expr::Grouping(e) => write!(f, "(group {})", e),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Expr::*;
    use super::*;
    use crate::token::Token;
    use crate::token::TokenType::{self, *};

    fn t(tt: TokenType, lexeme: &str) -> Box<Token> {
        use crate::token::Literal as L;

        Box::new(Token::new(tt, lexeme.into(), L::Null, 0, 0))
    }

    #[test]
    fn displays_ok() {
        let expr: Expr = Binary(
            Box::new(Binary(
                Box::new(Literal(t(NUMBER, "42"))),
                t(STAR, "*"),
                Box::new(Unary(t(MINUS, "-"), Box::new(Literal(t(NUMBER, "10"))))),
            )),
            t(PLUS, "+"),
            Box::new(Binary(
                Box::new(Literal(t(NUMBER, "100"))),
                t(SLASH, "/"),
                Box::new(Literal(t(NUMBER, "12"))),
            )),
        );

        assert_eq!(format!("{}", expr), "(+ (* 42 -10) (/ 100 12))");
    }
}
