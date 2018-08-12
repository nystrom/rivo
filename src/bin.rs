extern crate ivo;
use ivo::parser::parse_string;

fn main() {
    let t = parse_string(String::from("1+2"));
    println!("{:?}", *t);
}
