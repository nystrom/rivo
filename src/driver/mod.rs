use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;
use std::collections::HashMap;

use namer::graph::ScopeGraph;
use namer::graph::EnvIndex;
use namer::symbols::Scope;
use namer::symbols::Env;

// The interpreter states consists of the states of the bundles.
// One of the bundles is the current bundle being processed.
// The others are to-be-processed.

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BundleIndex(usize);

#[derive(Clone, Debug)]
pub struct Interpreter {
    pub current_bundle: Option<BundleIndex>,
    pub bundles: Vec<Bundle>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            current_bundle: None,
            bundles: vec![],
        }
    }

    pub fn new_bundle(&mut self, source: Source) -> BundleIndex {
        let index = self.bundles.len();
        self.bundles.push(Bundle::New { source });
        BundleIndex(index)
    }

    pub fn set_bundle(&mut self, index: BundleIndex, bundle: Bundle) {
        println!("set_bundle {:?} {:?}", index, &bundle);
        self.bundles[index.0] = bundle;
    }

    pub fn read_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use std::fs::File;
        use std::io::Read;
        use std::io::BufReader;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::New { source } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                let input = match source {
                    Source::NoSource => {
                        Err(Located::new(NO_LOC, "no source to parse".to_string()))
                    },
                    Source::FileSource(file) => {
                        match File::open(file) {
                            Ok(file) => {
                                let mut buf_reader = BufReader::new(file);
                                let mut input = String::new();
                                match buf_reader.read_to_string(&mut input) {
                                    Ok(_) => Ok(input),
                                    Err(msg) => Err(Located::new(NO_LOC, msg.to_string())),
                                }
                            },
                            Err(msg) => Err(Located::new(NO_LOC, msg.to_string())),
                        }
                    },
                    Source::StringSource(input) => {
                        Ok(input.clone())
                    },
                }?;

                self.set_bundle(index, Bundle::Read { source: source.clone(), input: input.clone() });

                self.current_bundle = old_bundle;
                Ok(())
            }
            _ => {
                Err(Located::new(NO_LOC, "already read".to_string()))
            }
        }
    }

    pub fn parse_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use parser::parse::Parser;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::New { source } => {
                self.read_bundle(index)?;
                self.parse_bundle(index)?;
                Ok(())
            },
            Bundle::Read { source, input } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                let mut parser = Parser::new(&source, input.as_str());
                let t = parser.parse_bundle()?;
                self.set_bundle(index, Bundle::Parsed { tree: t });

                self.current_bundle = old_bundle;

                Ok(())
            }
            _ => Err(Located::new(NO_LOC, "already parsed".to_string()))
        }
    }

    pub fn debug_lex_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use parser::parse::Parser;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::New { source } => {
                self.read_bundle(index)?;
                self.debug_lex_bundle(index)?;
                Ok(())
            },
            Bundle::Read { source, input } => {
                use parser::lex::Lexer;
                use parser::tokens::Token;
                use syntax::loc::Located;

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

                Ok(())
            }
            _ => Err(Located::new(NO_LOC, "already parsed".to_string()))
        }
    }

    pub fn get_bundle(&mut self, index: BundleIndex) -> Option<Bundle> {
        self.bundles.get(index.0).map(|b| b.clone())
    }

    pub fn enumerate_bundles<'a>(&'a self) -> Vec<(BundleIndex, &'a Bundle)> {
        self.bundles.iter().enumerate().map(|(i, b)| (BundleIndex(i), b)).collect()
    }

    pub fn load_bundle_by_name(&mut self, name: &Name) -> Result<BundleIndex, Located<String>> {
        let path = self.locate_bundle(name)?;
        self.load_from_path(path)
    }

    pub fn locate_bundle(&mut self, name: &Name) -> Result<String, Located<String>> {
        match name {
            Name::Id(x) => {
                let path = format!("{}.ivo", x);
                Ok(path)

                // FIXME: should search source path
                // FIXME: should mangle the identifier
            },
            _ => {
                Err(Located::new(NO_LOC, "not found".to_string()))
            },
        }
    }

    pub fn load_from_path(&mut self, path: String) -> Result<BundleIndex, Located<String>> {
        let index = self.new_bundle(Source::FileSource(path));
        self.read_bundle(index)?;
        Ok(index)
    }

    pub fn prename_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use namer::prename::Prenamer;
        use namer::prename::PrenameContext;
        use visit::rewrite::Rewriter;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::New { source } => {
                self.parse_bundle(index)?;
                self.prename_bundle(index)?;
                Ok(())
            },
            Bundle::Read { source, input } => {
                self.parse_bundle(index)?;
                self.prename_bundle(index)?;
                Ok(())
            },
            Bundle::Parsed { tree } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                // FIXME: we don't want to pay for the clone here.
                // need to clone the tree here so that we're no longer borrowing self
                // so we can borrow it mutably in the prenamer.
                let tree1 = tree.clone();

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

                    let new_bundle = Bundle::Prenamed { tree: tree1, graph: graph.clone(), scopes: scopes.clone() };
                    self.set_bundle(index, new_bundle);
                }

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
            Bundle::New { source } => {
                self.prename_bundle(index)?;
                self.name_bundle(index)?;
                Ok(())
            },
            Bundle::Read { source, input } => {
                self.prename_bundle(index)?;
                self.name_bundle(index)?;
                Ok(())
            },
            Bundle::Parsed { tree } => {
                self.prename_bundle(index)?;
                self.name_bundle(index)?;
                Ok(())
            },
            Bundle::Prenamed { tree, graph: g, scopes } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                let tree1 = tree.clone();
                let scopes1 = scopes.clone();

                {
                    let mut graph = g.clone();

                    let mut renamer = Renamer {
                        graph: &mut graph,
                        driver: self
                    };

                    renamer.visit_root(&tree1.value, &scopes1, &tree1.loc);
                    graph.solve(self)?;

                    let new_bundle = Bundle::Named { tree: tree1, envs: graph.get_envs(), scopes: scopes1 };
                    self.set_bundle(index, new_bundle);
                }


                self.current_bundle = old_bundle;

                Ok(())
            },
            _ => Err(Located::new(NO_LOC, "already named".to_string()))
        }
    }
}

#[derive(Clone, Debug)]
pub enum Bundle {
    New { source: Source },
    Read { source: Source, input: String },
    Parsed { tree: Located<Root> },
    Prenamed { tree: Located<Root>, graph: ScopeGraph, scopes: HashMap<NodeId, Scope> },
    Named { tree: Located<Root>, envs: Vec<Env>, scopes: HashMap<NodeId, Scope> },
    Core { envs: Vec<Env>, root_scope: Scope }
}

impl Bundle {
    pub fn new_env(&mut self) -> EnvIndex {
        match *self {
            Bundle::Named { ref mut tree, ref mut envs, ref mut scopes } => {
                let index = envs.len();
                envs.push(Env {
                    decls: Vec::new(),
                    imports: Vec::new(),
                    parents: Vec::new(),
                    includes: Vec::new(),
                });
                EnvIndex(index)
            },
            _ => unimplemented!(),
        }
    }
}
