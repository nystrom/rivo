extern crate ivo;

use ivo::parser::parse_string;
use ivo::syntax::visit::Rewriter;

struct SmokeTest;
impl<'a> Rewriter<'a> for SmokeTest {}

fn main() {
    let t = parse_string(String::from("1+2"));
    println!("{:?}", *t);

    let s = SmokeTest.visit_root(&*t);
    println!("{:?}", s);
}
