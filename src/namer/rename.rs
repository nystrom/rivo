use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;

use namer::symbols::*;
use namer::graph::*;
use namer::glr::*;
use driver;

use visit::rewrite::*;

use std::collections::HashMap;

// First pass:
// Create the scopes and initialize in the AST.
// Second pass: do the declarations, etc. Then call ScopeGraph.solve

impl<'a> Renamer<'a> {
    pub fn apply_mixfix(&self, tree: MixfixTree, es: Vec<Located<Exp>>) -> Option<Exp> {
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
                Exp::Name { name, id, lookup: Some(lookup) } => {
                    // return false if mixfix part
                    match self.cache.lookup_cache.get(&lookup) {
                        Some(decls) if ScopeGraph::all_mixfix(decls) => false,
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

pub struct Renamer<'a> {
    pub cache: &'a SolverResult,
    pub driver: &'a mut driver::Driver,
}

// visit any node that has a node id.
impl<'tables, 'a> Rewriter<'a, HashMap<NodeId, Scope>> for Renamer<'tables> {
    fn visit_exp(&mut self, s: &'a Exp, scopes: &HashMap<NodeId, Scope>, loc: &Loc) -> Exp {
        match s {
            Exp::Name { name, id, lookup: Some(lookup) } => {
                let new_node = self.walk_exp(s, scopes, loc);
                match self.cache.lookup_cache.get(&lookup) {
                    Some(trees) => new_node,
                    None => {
                        self.driver.error(Located::new(loc.clone(), format!("name {} not found in scope", name)));
                        new_node
                    },
                }
            },

            Exp::MixfixApply { es, id, lookup: Some(lookup) } => {
                let new_node = self.walk_exp(s, scopes, &loc);
                match self.cache.mixfix_cache.get(&lookup) {
                    Some(trees) if trees.len() == 1 => {
                        match self.apply_mixfix(trees.first().unwrap().clone(), es.clone()) {
                            Some(e) => e,
                            None => new_node,
                        }
                    },
                    Some(trees) => {
                        self.driver.error(Located::new(loc.clone(), "ambiguous mixfix expression".to_owned()));
                        new_node
                    }
                    None => {
                        self.driver.error(Located::new(loc.clone(), "cannot resolve mixfix expression".to_owned()));
                        new_node
                    },
                }
            },

            _ => {
                self.walk_exp(s, scopes, loc)
            },
        }
    }
}
