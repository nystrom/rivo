use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;
use std::collections::HashMap;

use namer::graph::ScopeGraph;
use namer::graph::EnvIndex;
use namer::symbols::Scope;
use namer::symbols::Env;

use self::bundle::*;
use self::loader::*;
use self::stats::*;

pub mod bundle;
pub mod errors;
pub mod loader;
pub mod stats;

#[cfg(debug_assertions)]
#[allow(non_upper_case_globals)]
static mut depth: u32 = 0;

// The interpreter states consists of the states of the bundles.
// One of the bundles is the current bundle being processed.
// The others are to-be-processed.

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BundleIndex(usize);

#[derive(Debug)]
pub struct Driver {
    pub current_bundle: Option<BundleIndex>,
    pub bundles: Vec<Bundle>,
    pub loader: Loader,
    pub stats: Stats,
    pub errors: Vec<Vec<Located<String>>>,
}

impl Driver {
    pub fn new() -> Driver {
        Driver {
            current_bundle: None,
            bundles: vec![],
            loader: Loader::new(),
            stats: Stats::new(),
            errors: vec![],
        }
    }

    pub fn dump_stats(&self) {
        self.stats.dump()
    }

    // Report an error. It is required that there be a current bundle with which to associate the errors.
    pub fn error(&mut self, msg: Located<String>) {
        match self.current_bundle {
            Some(BundleIndex(index)) => {
                if index < self.errors.len() {
                    self.errors[index].push(msg.clone());
                }
                else {
                    self.errors.resize(index+1, vec![]);
                    self.errors[index].push(msg.clone());
                }
            }
            None => {
                unreachable!();
            },
        }
    }

    fn dump_errors_for_bundle(&self, index: usize, bundle: &Bundle) -> u32 {
        let mut count = 0;

        if index < self.errors.len() {
            let mut errors = self.errors[index].clone();
            errors.sort_unstable_by(|a, b| a.loc.cmp(&b.loc));

            for Located { loc, value: msg } in errors {
                let decoded = bundle.decode_loc(loc);
                println!("{}:{}", decoded, msg);
                count += 1;
            }
        }

        count
    }

    pub fn dump_errors(&self) {
        let mut count = 0;
        for (i, b) in self.bundles.iter().enumerate() {
            count += self.dump_errors_for_bundle(i, b);
        }
        if count == 1 {
            println!("{} error.", count);
        }
        else if count > 1 {
            println!("{} errors.", count);
        }
        else {
            println!("Success.");
        }
    }

    #[cfg_attr(debug_assertions, trace)]
    pub fn set_bundle(&mut self, index: BundleIndex, bundle: Bundle) {
        // println!("set_bundle {:?} {:?}", index, &bundle);
        self.bundles[index.0] = bundle;
    }

    pub fn parse_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use parser::parse::Parser;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::Read { source, input } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                let timer = self.stats.start_timer();

                let mut parser = Parser::new(&source, input.as_str());

                match parser.parse_bundle() {
                    Ok(t) => {
                        let line_map = self.compute_line_map(&input);

                        self.stats.end_timer(&format!("parse_bundle({})", &source), timer);
                        self.stats.end_timer("parse_bundle", timer);
                        self.stats.accum("parse success", 1);

                        self.set_bundle(index, Bundle::Parsed { source: source.clone(), line_map, tree: t });
                        self.current_bundle = old_bundle;

                        Ok(())
                    },
                    Err(msg) => {
                        self.stats.end_timer(&format!("parse_bundle({})", &source), timer);
                        self.stats.end_timer("parse_bundle", timer);
                        self.stats.accum("parse failure", 1);

                        self.current_bundle = old_bundle;

                        Err(msg)
                    },
                }
            },
            _ => {
                Err(Located::new(NO_LOC, "already parsed".to_string()))
            },
        }
    }

    pub fn compute_line_map(&self, input: &String) -> LineMap {
        let mut v = vec![0];
        let mut cr = false;
        for (i, ch) in input.char_indices() {
            match ch {
                '\r' => {
                    cr = true;
                },
                '\n' => {
                    cr = false;
                    v.push((i+1) as u32);
                },
                _ => {
                    if cr {
                        v.push(i as u32);
                        cr = false;
                    }
                }
            }
        }
        LineMap { line_offsets: v }
    }

    pub fn debug_lex_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use parser::parse::Parser;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::Read { source, input } => {
                use parser::lex::Lexer;
                use parser::tokens::Token;
                use syntax::loc::Located;

                let line_map = self.compute_line_map(&input);

                let mut lex = Lexer::new(source, input.as_str());

                loop {
                    match lex.next_token() {
                        Ok(Located { loc: _, value: Token::EOF }) => {
                            break;
                        },
                        Ok(Located { loc, value }) => {
                            let decoded = line_map.decode(source.clone(), loc);
                            println!("{}:{:?}", decoded, value);
                        }
                        Err(Located { loc, value }) => {
                            return Err(Located { loc, value: "lexical error".to_string() })
                        }
                    }
                }

                Ok(())
            },
            _ => {
                Err(Located::new(NO_LOC, "already parsed".to_string()))
            },
        }
    }

    pub fn get_bundle(&mut self, index: BundleIndex) -> Option<Bundle> {
        self.bundles.get(index.0).map(|b| b.clone())
    }

    pub fn enumerate_bundles<'a>(&'a self) -> Vec<(BundleIndex, &'a Bundle)> {
        self.bundles.iter().enumerate().map(|(i, b)| (BundleIndex(i), b)).collect()
    }

    pub fn load_bundle_by_name(&mut self, name: &Name) -> Result<BundleIndex, Located<String>> {
        match self.loader.locate_bundle(name) {
            Ok(source) => {
                self.load_bundle_from_source(&source)
            },
            Err(msg) => {
                Err(Located { loc: NO_LOC, value: msg.to_string() })
            },
        }
    }

    pub fn load_bundle_from_source(&mut self, source: &Source) -> Result<BundleIndex, Located<String>> {
        match self.loader.load_source(source) {
            Ok(input) => {
                let index = self.bundles.len();
                self.bundles.push(Bundle::Read { source: source.clone(), input });
                Ok(BundleIndex(index))
            },
            Err(msg) => {
                Err(Located { loc: NO_LOC, value: msg.to_string() })
            },
        }
    }

    pub fn prename_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use namer::prename::Prenamer;
        use namer::prename::PrenameContext;
        use visit::rewrite::Rewriter;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::Read { source, input } => {
                self.parse_bundle(index)?;
                self.prename_bundle(index)?;
                Ok(())
            },
            Bundle::Parsed { source, line_map, tree } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                let timer = self.stats.start_timer();

                // FIXME: we don't want to pay for the clone here.
                // need to clone the tree here so that we're no longer borrowing self
                // so we can borrow it mutably in the prenamer.
                let tree1 = tree.clone();
                let source1 = source.clone();
                let line_map1 = line_map.clone();

                {
                    let mut scopes = HashMap::new();
                    let mut lookups = HashMap::new();
                    let mut lookups_here = HashMap::new();
                    let mut mixfixes = HashMap::new();

                    let mut graph = ScopeGraph::new();

                    let mut prenamer = Prenamer {
                        graph: &mut graph,
                        scopes: &mut scopes,
                        lookups: &mut lookups,
                        lookups_here: &mut lookups_here,
                        mixfixes: &mut mixfixes,
                        driver: self
                    };
                    prenamer.visit_root(&tree1.value, &PrenameContext::new(), &tree1.loc);

                    let new_bundle = Bundle::Prenamed { source: source1, line_map: line_map1, tree: tree1, graph: graph.clone(), scopes: scopes.clone() };
                    self.set_bundle(index, new_bundle);
                }

                self.stats.end_timer("prename_bundle", timer);

                self.current_bundle = old_bundle;

                Ok(())
            },
            _ => Err(Located::new(NO_LOC, "already prenamed".to_string()))
        }
    }

    pub fn name_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use namer::rename::Renamer;
        use visit::rewrite::Rewriter;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::Read { source, input } => {
                self.prename_bundle(index)?;
                self.name_bundle(index)?;
                Ok(())
            },
            Bundle::Parsed { source, line_map, tree } => {
                self.prename_bundle(index)?;
                self.name_bundle(index)?;
                Ok(())
            },
            Bundle::Prenamed { source, line_map, tree, graph, scopes } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                let timer = self.stats.start_timer();

                let tree1 = tree.clone();
                let scopes1 = scopes.clone();
                let source1 = source.clone();
                let line_map1 = line_map.clone();

                use namer::namer::Namer;
                use namer::namer::Cache;

                let mut cache = Cache {
                    lookup_here_cache: &mut HashMap::new(),
                    lookup_cache: &mut HashMap::new(),
                    mixfix_cache: &mut HashMap::new(),
                };

                let mut namer = Namer {
                    graph: &mut graph.clone(),
                    driver: self,
                    cache: &mut cache,
                };

                {
                    let result = namer.solve()?;

                    let graph1 = namer.graph.clone();

                    let mut renamer = Renamer {
                        cache: &result,
                        driver: self
                    };

                    renamer.visit_root(&tree1.value, &scopes1, &tree1.loc);

                    let new_bundle = Bundle::Named { source: source1, line_map: line_map1, tree: tree1, graph: graph1, scopes: scopes1 };
                    self.set_bundle(index, new_bundle);
                }

                self.stats.end_timer("name_bundle", timer);

                self.current_bundle = old_bundle;

                Ok(())
            },
            _ => Err(Located::new(NO_LOC, "already named".to_string()))
        }
    }
}
