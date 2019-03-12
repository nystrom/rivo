extern crate rivo;
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
    use rivo::syntax::loc::Source;

    let opt_input = {
        match matches.value_of("file.ivo") {
            Some(file) => {
                Some(Source::FileSource(std::path::PathBuf::from(file)))
            },
            None => {
                matches.value_of("expression").map(|input|
                    Source::StringSource(String::from(input)))
            },
        }
    };

    use rivo::driver::*;
    use rivo::driver::bundle::Bundle;
    let mut driver = Driver::new();

    let timer = driver.stats.start_timer();

    if let Some(source) = opt_input {
        match driver.load_bundle_from_source(&source) {
            Ok(bundle) => {
                if matches.is_present("lex") {
                    match driver.debug_lex_bundle(bundle) {
                        Ok(_) => {
                        },
                        Err(msg) => {
                            let b = driver.get_bundle(bundle).unwrap();
                            let loc = b.decode_loc(msg.loc);
                            println!("{}: lex error {}", loc, *msg);
                        }
                    }
                }
                else {
                    use rivo::syntax::pretty::*;

                    match driver.name_bundle(bundle) {
                        Ok(_) => {
                            let t = driver.get_bundle(bundle);
                            match t {
                                Some(Bundle::Named { tree: t, .. }) => {
                                    driver.stats.accum("named", 1);
                                    println!("{}", t.pretty(160));
                                },
                                _ => {
                                    driver.stats.accum("not named", 1);
                                },
                            }
                        },
                        Err(_msg) => {
                            assert!(driver.has_errors());
                            // let b = driver.get_bundle(bundle).unwrap();
                            // let loc = b.decode_loc(msg.loc);
                            // driver.error(Located::new(loc, "Compilation failed."));
                        },
                    }

                    // TODO: iterate over all the bundles and finish naming.
                }
            },
            Err(msg) => {
                println!("{}: {}", source, *msg);
            },
        }
    }
    else {
        println!("missing input expression or file");
    }

    driver.stats.end_timer("total time", timer);

    driver.dump_errors();
    driver.dump_stats();
}
