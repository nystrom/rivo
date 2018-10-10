extern crate ivo;
extern crate clap;

use clap::{Arg, App};

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
    use ivo::syntax::loc::Source;

    let opt_input = {
        match matches.value_of("file.ivo") {
            Some(file) => {
                Some(Source::FileSource(String::from(file)))
            },
            None => {
                matches.value_of("expression").map(|input|
                    Source::StringSource(String::from(input)))
            },
        }
    };

    use ivo::driver::*;
    let mut int = Interpreter::new();

    if let Some(source) = opt_input {
        let bundle = int.new_bundle(source.clone());

        if matches.is_present("lex") {
            match int.debug_lex_bundle(bundle) {
                Ok(_) => {
                },
                Err(msg) => {
                    println!("{}: lex error {}", msg.loc, *msg);
                }
            }
        }
        else {
            use ivo::syntax::pretty::*;

            match int.name_bundle(bundle) {
                Ok(_) => {
                    let t = int.get_bundle(bundle);
                    match t {
                        Some(Bundle::Prenamed { tree: t, .. }) => {
                            println!("{}", t.pretty(80));
                        },
                        Some(Bundle::Named { tree: t, .. }) => {
                            println!("{}", t.pretty(80));
                        },
                        _ => {},
                    }
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
