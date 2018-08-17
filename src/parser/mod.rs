use syntax::trees::Root;
use syntax::loc::Located;
use parser::parse::*;

pub fn parse_string(input: String) -> Result<Located<Root>, Located<String>> {
    let mut p = Parser::new("(unknown)", input.as_str());
    p.parse_bundle()
}

pub mod tokens;
pub mod lex;
pub mod parse;
