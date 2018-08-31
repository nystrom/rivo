extern crate ivo;
extern crate clap;

use std::fs::File;
use std::io::Read;
use clap::{Arg, App};

use ivo::parser::parse::Parser;
use ivo::visit::rewrite::Rewriter;

struct SmokeTest;
impl<'a> Rewriter<'a> for SmokeTest {}

fn main() {
    let matches = App::new("rivo")
                          .version("0.1.0")
                          .author("Nate Nystrom <nate.nystrom@usi.ch>")
                          .about("Rusty Ivo interpreter")
                          .arg(Arg::with_name("expression")
                               .short("e")
                               .long("expression")
                               .value_name("EXPRESSION")
                               .help("Evaluate the given expression")
                               .required(false)
                               .takes_value(true))
                          .arg(Arg::with_name("config")
                               .short("c")
                               .long("config")
                               .value_name("FILE")
                               .help("Sets a custom config file")
                               .takes_value(true))
                          .arg(Arg::with_name("file.ivo")
                               .help("Sets the input file to use")
                               .required(false)
                               .index(1))
                          .arg(Arg::with_name("v")
                               .short("v")
                               .long("verbose")
                               .multiple(true)
                               .help("Be more verbose"))
                          .arg(Arg::with_name("lex")
                               .short("l")
                               .long("lex")
                               .help("Test the lexer"))
                          .get_matches();

    // Gets a value for config if supplied by user, or defaults to "default.conf"
    let _config = matches.value_of("config").unwrap_or("ivo.conf");

    let opt_input = {
        match matches.value_of("file.ivo") {
            Some(file) => {
                let mut f = File::open(file).expect("file not found");
                let mut input = String::new();
                f.read_to_string(&mut input).expect("something went wrong reading the file");
                Some((file, input))
            },
            None => {
                matches.value_of("expression").map(|input|
                    ("(input)", String::from(input)))
            },
        }
    };

    if let Some((source, input)) = opt_input {
        if matches.is_present("lex") {
            use ivo::parser::lex::Lexer;
            use ivo::parser::tokens::Token;
            use ivo::syntax::loc::Located;

            let mut lex = Lexer::new(source, input.as_str());
            loop {
                match lex.next_token() {
                    Located { loc: _, value: Token::EOF } => {
                        break;
                    },
                    Located { loc, value } => {
                        println!("{}:{:?}", loc, value);
                    }
                }
            }
        }
        else {
            use ivo::syntax::pretty::*;

            let mut parser = Parser::new(source, input.as_str());
            let r = parser.parse_bundle();
            match r {
                Ok(t) => {
                    // println!("{:#?}", *t);
                    let s = SmokeTest.visit_root(&*t);
                    // println!("{:#?}", s);
                    println!("{}", s.pretty(80));
                },
                Err(msg) => {
                    println!("{}: parse error {}", msg.loc, *msg);
                },
            }
        }
    }
    else {
        println!("missing input expression or file");
    }
}
