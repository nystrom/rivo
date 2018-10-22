use num::bigint::BigInt;
use num::rational::BigRational;
use std::fmt;


#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    EOF,

    Arrow,
    Assign,
    At,
    BackArrow,
    Bang,
    Colon,
    Comma,
    Dot,
    Eq,
    Question,
    Semi,
    Tick,
    Underscore,

    Lb,
    Rb,
    Lc,
    Rc,
    Lp,
    Rp,

    Op(String),
    Id(String),

    For,
    Fun,
    Import,
    Native,
    Val,
    Var,
    Trait,
    With,
    Where,

    Char(char),
    Rat(BigRational, String),
    Int(BigInt, String),
    String(String),
}

pub struct TokenVec(pub Vec<Token>);

impl fmt::Display for TokenVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut sep = "";
        for t in &self.0 {
            write!(f, "{}", sep)?;
            write!(f, "{}", t)?;
            sep = ", "
        }
        Ok({})
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::EOF => write!(f, "end of file"),

            Token::Arrow => write!(f, "->"),
            Token::Assign => write!(f, ":="),
            Token::At => write!(f, "@"),
            Token::BackArrow => write!(f, "<-"),
            Token::Bang => write!(f, "!"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Eq => write!(f, "="),
            Token::Question => write!(f, "?"),
            Token::Semi => write!(f, ";"),
            Token::Tick => write!(f, "`"),
            Token::Underscore => write!(f, "_"),

            Token::Lb => write!(f, "["),
            Token::Rb => write!(f, "]"),
            Token::Lc => write!(f, "{}", "{"),
            Token::Rc => write!(f, "{}", "}"),
            Token::Lp => write!(f, "("),
            Token::Rp => write!(f, ")"),

            Token::Op(ref s) => write!(f, "operator {}", s),
            Token::Id(ref s) => write!(f, "identifier {}", s),

            Token::For => write!(f, "for"),
            Token::Fun => write!(f, "fun"),
            Token::Import => write!(f, "import"),
            Token::Native => write!(f, "native"),
            Token::Val => write!(f, "val"),
            Token::Var => write!(f, "var"),
            Token::Trait => write!(f, "trait"),
            Token::With => write!(f, "with"),
            Token::Where => write!(f, "where"),

            Token::Char(ref ch) => write!(f, "character literal '{}'", ch),
            Token::Rat(ref n, ref s) => write!(f, "rational literal {}", s),
            Token::Int(ref n, ref s) => write!(f, "integer literal {}", n),
            Token::String(ref s) => write!(f, "string literal \"{}\"", s),
        }
    }
}
