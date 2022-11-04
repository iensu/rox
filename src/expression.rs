use crate::token;

#[derive(PartialEq, Debug)]
pub enum Expr<'a> {
    Literal(&'a token::Value),
    Unary(&'a token::Token, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, &'a token::Token, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
}

impl<'a> std::fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use token::Value::*;
        match self {
            Expr::Literal(literal) => match literal {
                Null => write!(f, "null"),
                Bool(b) => write!(f, "{b}"),
                Number(n) => write!(f, "{n}"),
                String(s) => write!(f, "\"{s}\""),
                Identifier(i) => write!(f, "{i}"),
                Keyword(s) => write!(f, "{s}"),
            },
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
    use crate::token::Value as V;

    use test_log::test;

    fn t(tt: TokenType, lexeme: &str) -> Token {
        Token::new(tt, lexeme.into(), V::Null, 0, 0)
    }

    #[test]
    fn displays_ok() {
        let forty_two = V::Number(42.);
        let star = t(STAR, "*");
        let minus = t(MINUS, "-");
        let ten = V::Number(10.);
        let plus = t(PLUS, "+");
        let hundred = V::Number(100.);
        let slash = t(SLASH, "/");
        let twelve = V::Number(12.);

        let expr: Expr = Binary(
            Box::new(Binary(
                Box::new(Literal(&forty_two)),
                &star,
                Box::new(Unary(&minus, Box::new(Literal(&ten)))),
            )),
            &plus,
            Box::new(Binary(
                Box::new(Literal(&hundred)),
                &slash,
                Box::new(Literal(&twelve)),
            )),
        );

        assert_eq!(format!("{}", expr), "(+ (* 42 -10) (/ 100 12))");
        assert_eq!(
            format!("{}", Literal(&V::String("Hello".into()))),
            "\"Hello\""
        );
    }
}
