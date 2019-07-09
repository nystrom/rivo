use num::bigint::BigInt;
use num::rational::BigRational;
use std::fmt;


#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    EOF,

    Arrow,
    Assign,
    At,
    Backarrow,
    Colon,
    Comma,
    Dot,
    Eq,
    Hash,
    Semi,
    Tick,
    Underscore,
    
    Bang, // DEPRECATED
    Question, // DEPRECATED

    Lb,
    Rb,
    Lc,
    Rc,
    Lp,
    Rp,

    Op(String),
    Id(String),

    Enum,
    For,
    Fun,
    Import,
    Let,
    Module,
    Struct,
    Trait,
    Val, // DEPRECATED
    Var,
    With, // DEPRECATED
    Where, // DEPRECATED (replace with if)
    In, 
    Out, 
    Type, 

    // There should be removed once we have call-by-name working.
    If,
    Else,
    Match,
    While,
    Do,

    Char(char),
    Rat(BigRational, String),
    Int(BigInt, String),
    String(String),
}

pub struct TokenVec(pub Vec<Token>);

impl fmt::Display for TokenVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.len() {
            0 => Ok(()),
            1 => {
                write!(f, "{}", self.0[0])?;
                Ok(())
            },
            2 => {
                write!(f, "{} or {}", self.0[0], self.0[1])?;
                Ok(())
            }
            n => {
                for (i, t) in self.0.iter().enumerate() {
                    write!(f, "{}", t)?;
                    if i < n-2 {
                        write!(f, ", ")?;
                    }
                    else if i == n-2 {
                        write!(f, ", or ")?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::EOF => write!(f, "end of file"),

            Token::Arrow => write!(f, "`->`"),
            Token::Assign => write!(f, "`:=`"),
            Token::At => write!(f, "`@`"),
            Token::Backarrow => write!(f, "`<-`"),
            Token::Colon => write!(f, "`:`"),
            Token::Comma => write!(f, "`,`"),
            Token::Dot => write!(f, "`.`"),
            Token::Eq => write!(f, "`=`"),
            Token::Hash => write!(f, "`#`"),
            Token::Semi => write!(f, "`;`"),
            Token::Tick => write!(f, "`"),
            Token::Underscore => write!(f, "`_`"),

            // DEPRECATED
            Token::Bang => write!(f, "`!`"),
            Token::Question => write!(f, "`?`"),

            Token::Lb => write!(f, "`[`"),
            Token::Rb => write!(f, "`]`"),
            Token::Lc => write!(f, "`{{`"),
            Token::Rc => write!(f, "`}}`"),
            Token::Lp => write!(f, "`(`"),
            Token::Rp => write!(f, "`)`"),

            Token::Op(ref s) => write!(f, "operator `{}`", s),
            Token::Id(ref s) => write!(f, "identifier `{}`", s),

            Token::In => write!(f, "`in`"),
            Token::Out => write!(f, "`out`"),
            Token::Enum => write!(f, "`enum`"),
            Token::For => write!(f, "`for`"),
            Token::Fun => write!(f, "`fun`"),
            Token::Import => write!(f, "`import`"),
            Token::Let => write!(f, "`let`"),
            Token::Module => write!(f, "`module`"),
            Token::Struct => write!(f, "`struct`"),
            Token::Trait => write!(f, "`trait`"),
            Token::Val => write!(f, "`val`"),
            Token::Var => write!(f, "`var`"),
            Token::With => write!(f, "`with`"),
            Token::Where => write!(f, "`where`"),
            Token::Type => write!(f, "`type`"),

            Token::If => write!(f, "`if`"),
            Token::Else => write!(f, "`else`"),
            Token::Match => write!(f, "`match`"),
            Token::While => write!(f, "`while`"),
            Token::Do => write!(f, "`do`"),

            Token::Char(ref ch) => write!(f, "character literal `'{}'`", ch),
            Token::Rat(ref n, ref s) => write!(f, "rational literal `{}`", s),
            Token::Int(ref n, ref s) => write!(f, "integer literal `{}`", n),
            Token::String(ref s) => write!(f, "string literal `\"{}\"`", s),
        }
    }
}
