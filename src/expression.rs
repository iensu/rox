use crate::token;

#[derive(PartialEq, Debug)]
pub enum Expr<'a> {
    Assign(&'a token::Token<'a>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, &'a token::Token<'a>, Box<Expr<'a>>),
    Call {
        callee: Box<Expr<'a>>,
        closing_paren: &'a token::Token<'a>,
        args: Vec<Expr<'a>>,
    },
    Grouping(Box<Expr<'a>>),
    Literal(&'a token::Value),
    Logic(Box<Expr<'a>>, &'a token::Token<'a>, Box<Expr<'a>>),
    Unary(&'a token::Token<'a>, Box<Expr<'a>>),
    Variable(&'a token::Token<'a>),
}

impl<'a> std::fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Assign(t, e) => write!(f, "{} = {}", t.lexeme, e),
            Expr::Binary(l, op, r) => write!(f, "({} {} {})", op.lexeme, l, r),
            Expr::Call { callee, args, .. } => {
                write!(f, "{callee}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{arg}")?;
                    } else {
                        write!(f, ", {arg}")?;
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
            Expr::Grouping(e) => write!(f, "(group {})", e),
            Expr::Literal(literal) => write!(f, "{}", literal),
            Expr::Logic(l, op, r) => write!(f, "{l} {} {r}", op.lexeme),
            Expr::Unary(t, e) => write!(f, "{}{}", t.lexeme, e),
            Expr::Variable(v) => write!(f, "{}", v.lexeme),
        }
    }
}

pub enum Stmt<'a> {
    Block(Vec<Stmt<'a>>),
    Expression(Box<Expr<'a>>),
    If(Box<Expr<'a>>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
    Print(Box<Expr<'a>>),
    VarDecl(&'a token::Token<'a>, Option<Expr<'a>>),
    While(Box<Expr<'a>>, Box<Stmt<'a>>),
    Null, // FIXME: Do we need this?
}

impl<'a> std::fmt::Display for Stmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Block(stmts) => {
                write!(f, "{{ ")?;
                for stmt in stmts {
                    write!(f, "{}", stmt)?;
                }
                write!(f, " }}")?;
                Ok(())
            }
            Stmt::Expression(e) => write!(f, "{};", e),
            Stmt::If(condition, then_branch, else_branch) => {
                write!(f, "if ({condition}) {then_branch}")?;
                if let Some(branch) = else_branch {
                    write!(f, " else {branch}")?;
                }
                Ok(())
            }
            Stmt::Print(e) => write!(f, "print {};", e),
            Stmt::VarDecl(ident, expr) => match expr {
                Some(e) => write!(f, "var {} = {};", ident.lexeme, e),
                None => write!(f, "var {};", ident.lexeme),
            },
            Stmt::While(cond, body) => write!(f, "while ({cond}) {body}"),
            Stmt::Null => write!(f, "<null statement>"),
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

    fn l(value: &V) -> Expr {
        Literal(value)
    }

    #[test]
    fn expr_displays_ok() {
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

    #[test]
    fn stmt_displays_ok() {
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
        let stmt = Stmt::Expression(Box::new(expr));
        assert_eq!(format!("{}", stmt), "(+ (* 42 -10) (/ 100 12));");

        let hello = V::String("hello world".into());
        let stmt = Stmt::Print(Box::new(Literal(&hello)));
        assert_eq!(format!("{}", stmt), "print \"hello world\";");
    }

    #[test]
    fn call_displays_ok() {
        let foo = V::Identifier("foo".to_string());
        let hello = V::String("hello".to_string());
        let x = V::Identifier("x".to_string());
        let closing_paren = &t(RIGHT_PAREN, ")");

        let test_cases = [
            (
                Call {
                    callee: Box::new(l(&foo)),
                    closing_paren,
                    args: Vec::new(),
                },
                "foo()",
            ),
            (
                Call {
                    callee: Box::new(l(&foo)),
                    closing_paren,
                    args: vec![l(&x)],
                },
                "foo(x)",
            ),
            (
                Call {
                    callee: Box::new(l(&foo)),
                    closing_paren,
                    args: vec![l(&x), l(&V::Number(665.)), l(&V::Number(1.2)), l(&hello)],
                },
                r#"foo(x, 665, 1.2, "hello")"#,
            ),
        ];

        for (input, expected) in test_cases {
            assert_eq!(input.to_string(), expected)
        }
    }
}
