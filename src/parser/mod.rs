pub mod tokens;
pub mod lex;
pub mod trees;
pub mod parse;

pub mod either {
    pub enum Either<L, R> {
        Left(L),
        Right(R),
    }
}

pub mod grammar {
    include!(concat!(env!("OUT_DIR"), "/parser/grammar.rs"));
}
