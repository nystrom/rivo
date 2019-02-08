use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;

use namer::symbols::*;
use namer::graph::*;
use namer::namer::Namer;

use visit::rewrite::*;

use std::collections::HashMap;

use trace::trace;
trace::init_depth_var!();

pub struct Renamer<'a> {
    pub namer: &'a mut Namer<'a>,
    pub scopes: &'a HashMap<NodeId, Scope>,
    pub lookups: &'a HashMap<NodeId, LookupIndex>,
    pub mixfixes: &'a HashMap<NodeId, MixfixIndex>,
    pub node_id_generator: &'a mut NodeIdGenerator,
}

#[derive(Debug, Clone, Copy)]
pub struct RenamerCtx {
    in_mixfix: bool,
    // path: StablePath,
}

impl RenamerCtx {
    pub fn new() -> RenamerCtx {
        RenamerCtx {
            in_mixfix: false,
            // path: StablePath::Root,
        }
    }
}

impl<'a> Renamer<'a> {
    // fn convert_to_path(pat: &Located<Exp>) -> StablePath {
    //     match pat.value {
    //         Exp::Lit { lit } => StablePath::Lit { lit: lit.clone() },
    //         Exp::Select { exp, name } => StablePath::Select { outer: Renamer::convert_to_path(exp), name },
    //         Exp::Within { e1, e2 } => StablePath::Select { outer: Renamer::convert_to_path(e1), name },
    //         _ => StablePath::Unstable,
    //     }
    // }
    //
    // fn mk_path(p: StablePath, params: &[Param]) -> StablePath {
    //     if let Some((param, params)) = es.split_first() {
    //         let arg = match param {
    //             Param { attr: ParamAttr { mode: CallingMode::Input, .. }, pat } =>
    //                 Renamer::convert_to_path(pat),
    //             Param { attr: ParamAttr { mode: CallingMode::Output, .. }, pat } =>
    //                 StablePath::Lit { lit: Lit::Wildcard },
    //         };
    //         Renamer::mk_path(
    //             StablePath::Apply {
    //                 fun: Box::new(p),
    //                 arg: Box::new(arg),
    //             },
    //             params)
    //     }
    //     else {
    //         p
    //     }
    // }

    fn apply_mixfix(&mut self, tree: &MixfixTree, es: &Vec<Located<Exp>>) -> Option<Exp> {
        struct ParseResult {
            e: Located<Exp>,
            rest: Vec<Located<Exp>>
        }

        fn parse(node_id_generator: &mut NodeIdGenerator, t: &MixfixTree, es: Vec<Located<Exp>>) -> Option<ParseResult> {
            match t {
                MixfixTree::Name(x, decls) => {
                    let id = node_id_generator.new_id();
                    Some(
                        ParseResult {
                            // FIXME NodeId is wrong and lookup is wrong
                            e: Located::new(NO_LOC, Exp::Name { name: x.clone(), id: id }),
                            rest: es
                        }
                    )
                },
                MixfixTree::Apply(t1, t2) => {
                    parse(node_id_generator, t1, es).and_then(|ParseResult { e: e1, rest: rest1 }|
                        parse(node_id_generator, t2, rest1).map(|ParseResult { e: e2, rest: rest2 }|
                            ParseResult {
                                e: Located::new(NO_LOC, Exp::Apply { fun: Box::new(e1), arg: Box::new(e2) }),
                                rest: rest2
                            }
                        )
                    )
                },
                MixfixTree::Exp => {
                    match es.split_first() {
                        Some((e, es)) => Some(ParseResult { e: e.clone(), rest: es.to_vec() }),
                        None => None,
                    }
                },
            }
        }

        // filter out MixfixParts from es
        let vs = es.iter().filter(|e|
            match &e.value {
                Exp::Name { name, id } => {
                    // return false if mixfix part
                    let lookup = self.lookups.get(id).unwrap();
                    let lookup_ref = self.namer.driver.graph.get_lookup(lookup);

                    match self.namer.lookup(&lookup_ref) {
                        Ok(decls) => ! Namer::all_mixfix(&decls),
                        _ => true,
                    }
                }
                _e => true,
            }
        ).cloned().collect();

        match parse(&mut self.node_id_generator, tree, vs) {
            Some(ParseResult { ref e, ref rest }) if rest.is_empty() => Some(e.value.clone()),
            _ => None,
        }
    }

    fn lookup_by_id(&mut self, id: NodeId) -> Result<Vec<Located<Decl>>, Located<String>> {
        println!("id = {:?}", id);
        let lookup = self.lookups.get(&id).unwrap();
        println!("lookup = {:?}", lookup);

        match self.namer.driver.graph.get_pre_resolved(lookup) {
            Some(decls) => {
                println!("pre_resolved = {:?}", decls);
                Ok(decls.clone())
            },
            None => {
                let lookup_ref = self.namer.driver.graph.get_lookup(lookup);
                println!("lookup_ref = {:?}", lookup_ref);
                self.namer.lookup(&lookup_ref)
            },
        }
    }

    fn path_of_scope(&mut self, scope: Scope) -> StablePath {
        match scope {
            Scope::Global => StablePath::Root,
            Scope::Empty => StablePath::Unstable,
            Scope::Lookup(index) => StablePath::Unstable,
            Scope::Mixfix(index) => StablePath::Unstable,
            Scope::Env(index) => unimplemented!(),
            Scope::EnvWithoutImports(index) => unimplemented!(),
            Scope::EnvHere(index) => unimplemented!(),
        }
        // Root,
        // Fresh,
        // Unstable,
        // Lit { lit: Lit },
        // Select { outer: Box<Located<StablePath>>, name: Name },
        // Apply { fun: Box<Located<StablePath>>, arg: Box<Located<StablePath>> },

        // unimplemented!()
    }
}

// visit any node that has a node id.
impl<'tables, 'a> Rewriter<'a, RenamerCtx> for Renamer<'tables> {
    #[cfg_attr(debug_assertions, trace)]
    fn visit_def(&mut self, e: &'a Def, ctx: &RenamerCtx, loc: &Loc) -> Def {
        match e {
            Def::MixfixDef { id, attrs, flag, name, opt_guard, opt_body, params, ret } => {
                // let path = Renamer::mk_path(StablePath::Select { outer: ctx.path, name: *name }, &params);
                // let child_ctx = RenamerCtx {
                //     path: path,
                //     ..ctx.clone()
                // };
                //
                // // TODO:
                // // Map each scope to a path.
                // // Replace each Name with a Union of path selections.

                self.walk_def(e, ctx, loc)
            },
            _ => {
                self.walk_def(e, ctx, loc)
            }
        }
    }


    #[cfg_attr(debug_assertions, trace)]
    fn visit_exp(&mut self, e: &'a Exp, ctx: &RenamerCtx, loc: &Loc) -> Exp {
        match e {
            Exp::Outer { id } => {
                // Rewrite as the path to the containing scope.
                if let Some(scope) = self.scopes.get(&id) {
                    // TODO
                    let new_node = self.walk_exp(e, ctx, loc);
                    new_node
                }
                else {
                    self.namer.driver.error(Located::new(loc.clone(), format!("Could not find enclosing path of record. ({})", id)));
                    let new_node = self.walk_exp(e, ctx, loc);
                    new_node
                }
            }
            // Exp::Within { id, e1, e2 } => {
            //     // TODO: verify that e2 is in the scope of e1, not an enclosing scope.
            //     let new_node = self.walk_exp(e, ctx, loc);
            //
            //     // Rewrite e1.((f x) y) as ((e1.f) x) y
            //     match new_node {
            //         Exp::Within { id, e1, e2 } => {
            //             fn insert_select(e1: Located<Exp>, e2: Located<Exp>) -> Option<Located<Exp>> {
            //                 match e2 {
            //                     Exp::Name { id, name } => Some(Exp::Select { exp: e1, name }),
            //                     Exp::Apply { fun, arg } => {
            //                         insert_select(e1, fun).map(|fun| Exp::Apply { fun, arg })
            //                     },
            //                     _ => None,
            //                 }
            //             }
            //
            //             match insert_select(e1, e2) {
            //                 Some(e) => e,
            //                 None => {
            //                     self.namer.driver.error(Located::new(loc.clone(), format!("Could not resolve selected expression to a name or function application. ({})", id)));
            //                 },
            //             }
            //         }
            //         _ => new_node
            //     }
            // },
            Exp::Name { .. } => {
                let new_node = self.walk_exp(e, ctx, loc);

                // FIXME: generate union of selections rather than a name with a list of decls
                // Each scope has a path associated with it.

                match &new_node {
                    Exp::Name { name, id } => {
                        match self.lookup_by_id(*id) {
                            Ok(decls) => {
                                if decls.is_empty() {
                                    self.namer.driver.error(Located::new(loc.clone(), format!("Name {} not found in scope. ({})", name, id)));
                                }
                                else if ! ctx.in_mixfix && Namer::all_mixfix(&decls) {
                                    self.namer.driver.error(Located::new(loc.clone(), format!("Name {} is a mixfix part, not a name. ({})", name, id)));
                                }

                                for d in decls {
                                    let s = d.scope();
                                    let path = self.namer.driver.graph.get_path(s);
                                    println!("d {:?} path {:?}", d, path);
                                }

                                new_node
                            },
                            Err(_) => {
                                // Don't report... should have already been reported.
                                assert!(self.namer.driver.has_errors());
                                // self.namer.driver.error(Located::new(loc.clone(), format!("Name {} not found in scope (internal error too).", name)));
                                new_node
                            },
                        }
                    },
                    _ => new_node,
                }
            },

            Exp::MixfixApply { .. } => {
                self.namer.driver.stats.accum("mixfix rename", 1);

                let child_ctx = RenamerCtx {
                    in_mixfix: true,
                };

                let new_node = self.walk_exp(e, &child_ctx, &loc);

                match &new_node {
                    Exp::MixfixApply { es, id } => {
                        let lookup = self.mixfixes.get(id).unwrap();
                        let lookup_ref = self.namer.driver.graph.get_mixfix(lookup);

                        use crate::syntax::pretty::*;

                        match self.namer.parse_mixfix(&lookup_ref) {
                            Ok(trees) => {
                                match trees.first() {
                                    Some(tree) => {
                                        if trees.len() > 1 {
                                            let mut trees_clone = trees.clone();
                                            trees_clone.sort();
                                            self.namer.driver.error(Located::new(*loc, format!("Ambiguous mixfix expression: {}; resolves to one of {}. ({})", new_node.pretty(1000), MixfixTreeVec(&trees_clone), id)));
                                            new_node
                                        }
                                        else {
                                            match self.apply_mixfix(tree, es) {
                                                Some(e) => e,
                                                None => {
                                                    // This should only happen if there were other errors.
                                                    assert!(self.namer.driver.has_errors());
                                                    // self.namer.driver.error(Located::new(loc.clone(), "cannot resolve mixfix expression (internal error too)".to_owned()));
                                                    new_node
                                                },
                                            }
                                        }
                                    },
                                    None => {
                                        self.namer.driver.error(Located::new(*loc, format!("Cannot resolve mixfix expression: {}. ({})", new_node.pretty(1000), id)));
                                        new_node
                                    },
                                }
                            }
                            Err(msg) => {
                                self.namer.driver.error(Located::new(*loc, format!("Cannot resolve mixfix expression: {}. {}. ({})", new_node.pretty(1000), msg.value, id)));
                                new_node
                            },
                        }
                    },
                    _ => new_node,
                }
            },

            _ => {
                let ctx1 = RenamerCtx {
                    in_mixfix: false,
                };

                self.walk_exp(e, &ctx1, loc)
            },
        }
    }
}
