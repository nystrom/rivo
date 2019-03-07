use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;
use std::collections::HashMap;

use namer::graph::ScopeGraph;
use namer::graph::EnvIndex;

use self::bundle::*;
use self::loader::*;
use self::stats::*;

pub mod bundle;
pub mod loader;
pub mod stats;

use trace::trace;
trace::init_depth_var!();

// The interpreter states consists of the states of the bundles.
// One of the bundles is the current bundle being processed.
// The others are to-be-processed.

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BundleIndex(usize);

#[derive(Debug)]
pub struct Driver {
    pub current_bundle: Option<BundleIndex>,
    pub bundles: Vec<Bundle>,
    pub loader: Loader,
    pub stats: Stats,
    pub errors: Vec<Vec<Located<String>>>,
    pub graph: ScopeGraph,
}

impl Driver {
    pub fn new() -> Driver {
        Driver {
            current_bundle: None,
            bundles: vec![],
            loader: Loader::new(),
            stats: Stats::new(),
            errors: vec![],
            graph: ScopeGraph::new(),
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
                println!("{}: {}", decoded, msg);
                count += 1;
            }
        }

        count
    }

    fn error_count_for_bundle(&self, index: usize, bundle: &Bundle) -> u32 {
        if index < self.errors.len() {
            let errors = &self.errors[index];
            errors.len() as u32
        }
        else {
            0
        }
    }

    fn bundle_has_errors(&self, index: usize, bundle: &Bundle) -> bool {
        if index < self.errors.len() {
            let errors = &self.errors[index];
            ! errors.is_empty()
        }
        else {
            false
        }
    }

    pub fn has_errors(&self) -> bool {
        for (i, b) in self.bundles.iter().enumerate() {
            if self.bundle_has_errors(i, b) {
                return true;
            }
        }
        false
    }

    pub fn error_count(&self) -> u32 {
        let mut count = 0;
        for (i, b) in self.bundles.iter().enumerate() {
            count += self.error_count_for_bundle(i, b);
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

    pub fn set_bundle(&mut self, index: BundleIndex, bundle: Bundle) {
        println!("set_bundle {:?} {:#?}", index, &bundle);
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

                let mut node_id_generator = NodeIdGenerator::new();
                let mut parser = Parser::new(&source, input.as_str(), &mut node_id_generator);

                match parser.parse_bundle() {
                    Ok(t) => {
                        self.stats.end_timer(&format!("parse_bundle({})", &source), timer);
                        self.stats.end_timer("parse_bundle", timer);
                        self.stats.accum("parse success", 1);

                        let line_map = LineMap::new(&input);
                        self.set_bundle(index, Bundle::Parsed { source: source.clone(), line_map, node_id_generator, tree: t });
                        self.current_bundle = old_bundle;

                        Ok(())
                    },
                    Err(msg) => {
                        self.stats.end_timer(&format!("parse_bundle({})", &source), timer);
                        self.stats.end_timer("parse_bundle", timer);
                        self.stats.accum("parse failure", 1);

                        // To get correct error messasges, we need the line map.
                        let line_map = LineMap::new(&input);

                        // We have to clone the errors before mutating self with set_bundle.
                        let errs = parser.errors.clone();

                        self.set_bundle(index, Bundle::Parsed { source: source.clone(), line_map, node_id_generator, tree: Located::new(Loc::no_loc(), Root::Bundle { id: NodeId(0), cmds: vec![] }) });

                        for err in errs {
                            self.error(err);
                        }

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

    pub fn debug_lex_bundle(&mut self, index: BundleIndex) -> Result<(), Located<String>> {
        use parser::parse::Parser;

        let bundle = &self.bundles[index.0];

        match bundle {
            Bundle::Read { source, input } => {
                use parser::lex::Lexer;
                use parser::tokens::Token;
                use syntax::loc::Located;

                let line_map = LineMap::new(&input);

                let mut lex = Lexer::new(source, input.as_str());

                loop {
                    match lex.next_token() {
                        Ok(Located { loc, value: Token::EOF }) => {
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
        self.bundles.get(index.0).cloned()
    }

    pub fn enumerate_bundles<'a>(&'a self) -> Vec<(BundleIndex, &'a Bundle)> {
        self.bundles.iter().enumerate().map(|(i, b)| (BundleIndex(i), b)).collect()
    }

    pub fn load_bundle_by_name(&mut self, name: Name) -> Result<BundleIndex, Located<String>> {
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
        use namer::prename::PrenameCtx;
        use visit::rewrite::Rewriter;

        let bundle = &self.bundles[index.0];

        // TODO:
        // Rather than borrow the bundle from the driver, take ownership (removing it from the driver)
        // Then give it back to the driver.

        match bundle {
            Bundle::Read { .. } => {
                self.parse_bundle(index)?;
                self.prename_bundle(index)?;
                Ok(())
            },
            Bundle::Parsed { source, line_map, node_id_generator, tree } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                let timer = self.stats.start_timer();

                // FIXME: we don't want to pay for the clone here.
                // need to clone the tree here so that we're no longer borrowing self
                // so we can borrow it mutably in the prenamer.
                let tree1 = tree.clone();
                let source1 = source.clone();
                let line_map1 = line_map.clone();
                let mut node_id_generator1 = *node_id_generator;

                {
                    let mut prenamer = Prenamer {
                        scopes: &mut HashMap::new(),
                        lookups: &mut HashMap::new(),
                        mixfixes: &mut HashMap::new(),
                        node_id_generator: &mut node_id_generator1,
                        bundle: index,
                        driver: self
                    };

                    let tree2 = prenamer.visit_root(&tree1.value, &PrenameCtx::new(), tree1.loc);

                    let new_bundle = Bundle::Prenamed {
                        source: source1,
                        line_map: line_map1,
                        node_id_generator: *prenamer.node_id_generator,
                        tree: Located::new(tree1.loc, tree2),
                        scopes: prenamer.scopes.clone(),
                        lookups: prenamer.lookups.clone(),
                        mixfixes: prenamer.mixfixes.clone(),
                    };

                    println!("scopes {:#?}", prenamer.scopes);
                    // We can't print the graph until after the last use of the prenamer (which mutably borrows self)
                    println!("graph {:#?}", self.graph);

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
            Bundle::Read { .. } => {
                self.prename_bundle(index)?;
                self.name_bundle(index)?;
                Ok(())
            },
            Bundle::Parsed { .. } => {
                self.prename_bundle(index)?;
                self.name_bundle(index)?;
                Ok(())
            },
            Bundle::Prenamed { source, line_map, node_id_generator, tree, scopes, lookups, mixfixes } => {
                let old_bundle = self.current_bundle;
                self.current_bundle = Some(index);

                let timer = self.stats.start_timer();

                // FIXME: avoid the cloning
                // We clone so that after the cloning we're no longer borrowing &self immutably.
                let tree1 = tree.clone();
                let scopes1 = scopes.clone();
                let source1 = source.clone();
                let line_map1 = line_map.clone();
                let lookups1 = lookups.clone();
                let mixfixes1 = mixfixes.clone();
                let mut node_id_generator1 = *node_id_generator;

                use namer::namer::Namer;
                use namer::rename::RenamerCtx;

                let mut namer = Namer::new(self);

                {
                    let mut renamer = Renamer {
                        namer: &mut namer,
                        scopes: &scopes1,
                        lookups: &lookups1,
                        mixfixes: &mixfixes1,
                        node_id_generator: &mut node_id_generator1,
                    };

                    let tree2 = renamer.visit_root(&tree1.value, &RenamerCtx::new(), tree1.loc);

                    let new_bundle = Bundle::Named { source: source1, line_map: line_map1, node_id_generator: *renamer.node_id_generator, tree: Located::new(tree1.loc, tree2), scopes: scopes1 };
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
