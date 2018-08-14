use num::bigint::BigInt;
use num::rational::BigRational;

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

  Lb, Rb,
  Lc, Rc,
  Lp, Rp,

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

  Char(char),
  Rat(BigRational, String),
  Int(BigInt, String),
  String(String),

  BadChar,
  BadString,
  BadInt,
  BadRat,
  BadComment,
  UnexpectedChar(char),
}
