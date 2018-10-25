use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;

use namer::symbols::*;
use namer::glr::*;
use namer::graph::*;
use namer::namer::Namer;

use visit::rewrite::*;

use std::collections::HashMap;

#[cfg(debug_assertions)]
#[allow(non_upper_case_globals)]
static mut depth: u32 = 0;

pub struct Renamer<'a> {
    pub namer: &'a mut Namer<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct RenamerEnv {
    in_mixfix: bool,
}

impl RenamerEnv {
    pub fn new() -> RenamerEnv {
        RenamerEnv {
            in_mixfix: false,
        }
    }
}

impl<'a> Renamer<'a> {
    pub fn apply_mixfix(&mut self, tree: MixfixTree, es: Vec<Located<Exp>>) -> Option<Exp> {
        struct ParseResult {
            e: Located<Exp>,
            rest: Vec<Located<Exp>>
        }

        fn parse(t: MixfixTree, es: Vec<Located<Exp>>) -> Option<ParseResult> {
            match t {
                MixfixTree::Name(x, decls) =>
                    Some(
                        ParseResult {
                            // FIXME NodeId is wrong and lookup is wrong
                            e: Located::new(NO_LOC, Exp::Name { name: x, id: NodeId(0), lookup: None }),
                            rest: es
                        }
                    ),
                MixfixTree::Apply(t1, t2) =>
                    parse(*t1, es).and_then(|ParseResult { e: e1, rest: rest1 }|
                        parse(*t2, rest1).map(|ParseResult { e: e2, rest: rest2 }|
                            ParseResult {
                                e: Located::new(NO_LOC, Exp::Apply { fun: Box::new(e1), arg: Box::new(e2) }),
                                rest: rest2
                            }
                        )
                    ),
                MixfixTree::Exp =>
                    match es.split_first() {
                        Some((e, es)) => Some(ParseResult { e: e.clone(), rest: es.to_vec() }),
                        None => None,
                    },
            }
        }

        // filter out MixfixParts from es
        let vs = es.iter().filter(|e|
            match &e.value {
                Exp::Name { name, id, lookup: Some(LookupIndex(index)) } => {
                    // return false if mixfix part
                    let lookup = self.namer.graph.lookups.get(*index).unwrap().clone();

                    match self.namer.lookup(&lookup) {
                        Ok(decls) => ! Namer::all_mixfix(&decls),
                        _ => true,
                    }
                }
                _e => true,
            }
        ).cloned().collect();

        match parse(tree, vs) {
            Some(ParseResult { ref e, ref rest }) if rest.is_empty() => Some(e.value.clone()),
            _ => None,
        }
    }
}

// visit any node that has a node id.
impl<'tables, 'a> Rewriter<'a, RenamerEnv> for Renamer<'tables> {
    #[cfg_attr(debug_assertions, trace)]
    fn visit_exp(&mut self, e: &'a Exp, env: &RenamerEnv, loc: &Loc) -> Exp {
        match e {
            Exp::Name { .. } => {
                let new_node = self.walk_exp(e, env, loc);

                match &new_node {
                    Exp::Name { name, id, lookup: Some(LookupIndex(index)) } => {
                        let lookup = self.namer.graph.lookups.get(*index).unwrap().clone();
                        println!("NAME {:?}", name);

                        match self.namer.lookup(&lookup) {
                            Ok(decls) => {
                                if decls.is_empty() {
                                    self.namer.driver.error(Located::new(loc.clone(), format!("Name {} not found in scope.", name)));
                                }
                                else if Namer::all_mixfix(&decls) && ! env.in_mixfix {
                                    self.namer.driver.error(Located::new(loc.clone(), format!("Name {} is a mixfix part, not a name.", name)));
                                }
                                else {
                                    println!("{:#?}", decls);
                                }
                                new_node
                            },
                            Err(_) => {
                                self.namer.driver.error(Located::new(loc.clone(), format!("Name {} not found in scope (internal error too).", name)));
                                new_node
                            },
                        }
                    },
                    _ => new_node,
                }
            },

            Exp::MixfixApply { .. } => {
                self.namer.driver.stats.accum("mixfix rename", 1);

                let env1 = RenamerEnv {
                    in_mixfix: true,
                };

                let new_node = self.walk_exp(e, &env1, &loc);

                match &new_node {
                    Exp::MixfixApply { es, id, lookup: Some(MixfixIndex(index)) } => {
                        let lookup = self.namer.graph.mixfixes.get(*index).unwrap().clone();

                        match self.namer.parse_mixfix(&lookup) {
                            Ok(trees) => {
                                if trees.len() == 1 {
                                    match self.apply_mixfix(trees.first().unwrap().clone(), es.clone()) {
                                        Some(e) => e,
                                        None => {
                                            self.namer.driver.error(Located::new(loc.clone(), "cannot resolve mixfix expression (internal error too)".to_owned()));
                                            new_node
                                        },
                                    }
                                }
                                else {
                                    self.namer.driver.error(Located::new(loc.clone(), "ambiguous mixfix expression".to_owned()));
                                    new_node
                                }
                            }
                            Err(_) => {
                                self.namer.driver.error(Located::new(loc.clone(), "cannot resolve mixfix expression".to_owned()));
                                new_node
                            },
                        }
                    },
                    _ => new_node,
                }
            },

            _ => {
                let env1 = RenamerEnv {
                    in_mixfix: false,
                };

                self.walk_exp(e, &env1, loc)
            },
        }
    }
}
