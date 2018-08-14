use syntax::trees::{Root, Cmd, Exp, Lit};
use syntax::loc::{Located, NO_LOC};

pub fn parse_string(input: String) -> Located<Root> {
    let e = Located {
        loc : NO_LOC,
        value : Cmd::Exp(Exp::Literal { lit: Lit::Nothing })
    };

    Located {
        loc : NO_LOC,
        value : Root::Parsed { cmds: vec!(e) }
    }
}

mod tokens;
mod lex;
mod parse;
