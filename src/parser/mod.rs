use syntax::trees::{Tree, Lit};
use syntax::loc::{Located, NO_LOC};

pub fn parse(_input: String) -> Located<Tree> {
    Located {
        loc : NO_LOC,
        value : Tree::Literal { lit: Lit::Nothing }
    }
}
