// ScopeGraph maintains the scope graph.

use syntax::loc::*;
use syntax::names::*;
use syntax::trees;

use namer::symbols::*;
use namer::glr;

use driver;
use driver::*;
use driver::bundle::*;

use super::graph::*;
use super::glr_adapt::*;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use rpds::HashTrieSet;

#[cfg(debug_assertions)]
#[allow(non_upper_case_globals)]
static mut depth: u32 = 0;

type Crumbs = HashTrieSet<Scope>;

// internal cache
#[derive(Debug)]
pub struct Cache<'a> {
    pub lookup_cache: &'a mut HashMap<LookupRef, Vec<Located<Decl>>>,
    pub mixfix_cache: &'a mut HashMap<MixfixRef, Vec<MixfixTree>>
}

type LookupResult<T> = Result<T, Located<String>>;

// TODO: use this instead of the impl below
pub struct Namer<'a> {
    pub graph: &'a ScopeGraph,
    pub driver: &'a mut driver::Driver,
    pub cache: &'a mut Cache<'a>,
}

impl<'a> Namer<'a> {
    pub fn lookup(&mut self, r: &LookupRef) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupRef { scope, name, follow_parents } => {
                self.lookup_in_scope(*scope, &name, *follow_parents)
            }
        }
    }

    pub fn parse_mixfix(&mut self, r: &MixfixRef) -> LookupResult<Vec<MixfixTree>> {
        match r {
            MixfixRef { parts } => {
                self.resolve_mixfix(&parts)
            }
        }
    }

    fn do_lookup(&mut self, r: LookupRef) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupRef { scope, name, follow_parents } => self.lookup_in_scope(scope, &name, follow_parents)
        }
    }

    fn do_mixfix(&mut self, r: MixfixRef) -> LookupResult<Vec<MixfixTree>> {
        match r {
            MixfixRef { parts } => self.resolve_mixfix(&parts)
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn resolve_mixfix(&mut self, parts: &Vec<MixfixPart>) -> LookupResult<Vec<MixfixTree>> {
        match self.cache.mixfix_cache.get(&MixfixRef { parts: parts.clone() }) {
            Some(trees) => {
                println!("mixfix cache hit");
                self.driver.stats.accum("mixfix cache hit", 1);
                return Ok(trees.clone())
            },
            None => {
                println!("mixfix cache miss");
            },
        }

        self.driver.stats.accum("mixfix expression size", parts.len() as u64);

        use crate::namer::glr::Token;

        let mut refs: Vec<Option<LookupRef>> = vec![];

        for part in parts {
            match part.name_ref {
                Some(LookupIndex(index)) => {
                    let r = self.graph.lookups.get(index);
                    match r {
                        Some(r) => refs.push(Some(r.clone())),
                        None => refs.push(None),
                    }
                },
                None => {
                    refs.push(None);
                },
            }
        }

        let mut tokens = vec![];
        let mut has_name = false;

        for r in refs {
            match r {
                Some(r) => {
                    let decls = self.do_lookup(r.clone())?;

                    if ! decls.is_empty() && Namer::all_mixfix(&decls) {
                        match &r.name {
                            Name::Id(x) => {
                                tokens.push(Token::Name(Part::Id(x.to_owned()), decls.iter().map(|l| l.value.clone()).collect()));
                                has_name = true;
                            },
                            Name::Op(x) => {
                                tokens.push(Token::Name(Part::Op(x.to_owned()), decls.iter().map(|l| l.value.clone()).collect()));
                                has_name = true;
                            },
                            _ => {
                                tokens.push(Token::Exp(MixfixTree::Exp));
                            },
                        }
                    }
                    else {
                        // Either the name resolved at least one non-mixfix declaration
                        // or it resolved to nothing. In the latter case, we'll get an error later during
                        // renaming, but treat it like an expression so mixfix parsing might
                        // possibly succeed and we won't report an error for that too.
                        tokens.push(Token::Exp(MixfixTree::Exp));
                    }
                },
                None => {
                    tokens.push(Token::Exp(MixfixTree::Exp));
                },
            }
        }

        if ! has_name {
            // FIXME: MixfixParser should handle this case already!
            // If there are no names in the mixfix expression, we'll
            // fail, but we can turn it into a call expression.
            let mut ts = tokens.iter();
            let tree: MixfixTree = match ts.next() {
                Some(Token::Exp(tree)) => ts.fold(tree.clone(), |left, t|
                    match t {
                        Token::Exp(tree) => MixfixTree::Apply(Box::new(left.clone()), Box::new(tree.clone())),
                        _ => unreachable!(),
                    }
                ),
                _ => unreachable!(),
            };

            self.driver.stats.accum("mixfix without name", 1);

            Ok(vec![tree])
        }
        else {
            // Resolve the mixfix expression.

            // NOTE: do not add the $ token to the input.. GLR will do that for us.
            // tokens.push(Token::End);

            let decls: Vec<Decl> = tokens.iter().flat_map(|token| {
                match token {
                    Token::Name(_, decls) => {
                        decls.iter().map(|decl|
                            match decl {
                                Decl::MixfixPart {
                                    orig, ..
                                } => *orig.clone(),
                                decl => decl.clone(),
                            }
                        ).collect()
                    },
                    _ => vec![]
                }
            }).collect();

            println!("parts {:?}", parts);
            println!("tokens {:?}", tokens);
            println!("decls {:?}", decls);

            self.driver.stats.accum("mixfix decls size", decls.len() as u64);

            let mut parser = GLRAdapter::glr_from_decls(&decls);

            Ok(parser.parse(tokens))
        }
    }


    #[cfg_attr(debug_assertions, trace(disable(driver)))]
    fn lookup_in_root(&mut self, name: &Name) -> LookupResult<Vec<Located<Decl>>> {
        self.driver.stats.accum("lookup_in_root", 1);

        // we only search other bundles for legal bundle names
        if ! name.is_bundle_name() {
            self.driver.stats.accum("lookup_in_root not bundle name", 1);
            return Ok(vec![]);
        }

        let mut results = self.lookup_in_trees(name)?;

        if Namer::all_mixfix(&results) {
            match self.load_bundle_by_name(name) {
                Ok(index) => {
                    match self.driver.name_bundle(index) {
                        Ok(_) => {},
                        Err(msg) => {
                            return Err(msg)
                        },
                    }

                    // FIXME: the Cache should be part of the bundle if named.
                    let cache = Cache {
                        lookup_cache: &mut HashMap::new(),
                        mixfix_cache: &mut HashMap::new(),
                    };

                    match self.driver.get_bundle(index) {
                        Some(Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let mut v = self.lookup_in_scope(*scope, name, false)?;
                                results.append(&mut v);
                            }
                            else {
                                self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                            }
                        },
                        Some(Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let mut v = self.lookup_in_scope(*scope, name, false)?;
                                results.append(&mut v);
                            }
                            else {
                                self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                            }
                        },
                        Some(Bundle::Core { root_scope, .. }) => {
                            let mut v = self.lookup_in_scope(root_scope, name, false)?;
                            results.append(&mut v);
                        },
                        _ => {},
                    }
                },
                Err(msg) => {
                    // There is no bundle with the given name, but ignore that since it's not
                    // required to exist.
                },
            }
        }

        Ok(results)
    }

    #[cfg_attr(debug_assertions, trace(disable(driver)))]
    fn load_bundle_by_name(&mut self, name: &Name) -> LookupResult<BundleIndex> {
        self.driver.load_bundle_by_name(name)
    }

    #[cfg_attr(debug_assertions, trace(disable(driver)))]
    fn lookup_in_trees(&mut self, name: &Name) -> LookupResult<Vec<Located<Decl>>> {
        self.driver.stats.accum("lookup_in_trees", 1);

        let mut results = Vec::new();
        let mut to_name = Vec::new();

        // bring any loaded bundles up to the naming phase.
        // we can't do the naming in this loop because of borrowing constraints;
        // instead we copy to another vector and then do the naming.
        for (index, bundle) in self.driver.enumerate_bundles() {
            if Some(index) != self.driver.current_bundle {
                match bundle {
                    Bundle::Read { .. } => {
                        to_name.push(index);
                    },
                    Bundle::Parsed { .. } => {
                        to_name.push(index);
                    },
                    _ => {
                        // already named
                    },
                }
            }
        }

        self.driver.stats.accum("lookup_in_trees", 1);

        for index in to_name {
            println!("naming bundle {:?} to find {:?}", index, name);

            self.driver.stats.accum("naming in lookup_in_trees", 1);

            match self.driver.name_bundle(index) {
                Ok(_) => {},
                Err(msg) => {
                    // return Err(msg)
                },
            }
        }

        // Assert that we named things?
        for (index, bundle) in self.driver.enumerate_bundles() {
            if Some(index) != self.driver.current_bundle {
                match bundle {
                    Bundle::Read { .. } => {
                        assert!(false);
                    },
                    Bundle::Parsed { .. } => {
                        assert!(false);
                    },
                    _ => {
                        // already named
                    },
                }
            }
        }

        // HACK
        // We want to call lookup_in_scope, but can't since we have to mutably borrow driver
        // while we're looping over it.
        let stats = &mut self.driver.stats;

        for bundle in &self.driver.bundles {
            match bundle {
                Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
                    if let Some(Scope::Env(env)) = scopes.get(&id) {
                        let mut v = Namer::lookup_here_in_env(stats, graph, *env, name)?;
                        results.append(&mut v);
                    }
                },
                Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
                    if let Some(Scope::Env(env)) = scopes.get(&id) {
                        let mut v = Namer::lookup_here_in_env(stats, graph, *env, name)?;
                        results.append(&mut v);
                    }
                },
                // Bundle::Core { root_scope, graph, .. } => {
                //     to_search.push(*root_scope);
                // },
                _ => {},
            }
        }

        // let mut to_search = vec![];
        //
        // for bundle in &driver.bundles {
        //     match bundle {
        //         Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
        //             if let Some(scope) = scopes.get(&id) {
        //                 to_search.push((graph, scope));
        //             }
        //         },
        //         Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
        //             if let Some(scope) = scopes.get(&id) {
        //                 to_search.push((graph, scope));
        //             }
        //         },
        //         _ => {},
        //     }
        // }
        //
        // for (graph, scope) in to_search {
        //     let mut v = graph.lookup_here_in_scope(driver, *scope, name, &Cache { lookup_here_cache: &HashMap::new(), lookup_cache: &HashMap::new(), mixfix_cache: &HashMap::new() })?;
        //     results.append(&mut v);
        // }

        self.driver.stats.accum("lookup_in_trees size", results.len() as u64);

        Ok(results)
    }

    #[cfg_attr(debug_assertions, cfg_attr(debug_assertions, trace(disable(driver, cache))))]
    fn lookup_in_imports(&mut self, imports: &Vec<Located<Import>>, x: &Name) -> LookupResult<Vec<Located<Decl>>> {
        let include_self: Vec<(Scope, Name)> = imports.iter().filter_map(|import| {
            match import {
                Located { loc, value: Import::Here { path: scope } } =>
                    Some((scope.clone(), x.clone())),
                _ =>
                    None,
            }
        }).collect();

        let exclude_all: Vec<(Scope, Name)> = imports.iter().filter_map(|import| {
            match import {
                Located { loc, value: Import::None { path: scope } } =>
                    Some((scope.clone(), x.clone())),
                _ =>
                    None,
            }
        }).collect();

        let include_all: Vec<(Scope, Name)> = imports.iter().filter_map(|import| {
            match import {
                Located { loc, value: Import::All { path: scope } } =>
                    Some((scope.clone(), x.clone())),
                _ =>
                    None,
            }
        }).collect();

        let include: Vec<(Scope, Name)> = imports.iter().filter_map(|import| {
            match import {
                Located { loc, value: Import::Including { path: scope, name: y } } if y == x =>
                    Some((scope.clone(), x.clone())),
                Located { loc, value: Import::Renaming { path: scope, name: y, rename: z } } if z == x =>
                    Some((scope.clone(), y.clone())),
                _ =>
                    None,
            }
        }).collect();

        let exclude: Vec<(Scope, Name)> = imports.iter().filter_map(|import| {
            match import {
                Located { loc, value: Import::Excluding { path: scope, name: y } } if y == x =>
                    Some((scope.clone(), x.clone())),
                _ =>
                    None,
            }
        }).collect();

        let mut paths: Vec<(Scope, Name)> = Vec::new();
        for (p, x) in include_all {
            paths.push((p, x));
        }
        for (p, x) in exclude_all {
            paths.remove_item(&(p, x));
        }
        for (p, x) in exclude {
            paths.remove_item(&(p, x));
        }
        for (p, x) in include {
            paths.push((p, x));
        }

        paths.dedup();

        let mut results = Vec::new();

        // FIXME: clarify this! We want to lookup p.x. When resolving _p_, we should not search the other imports.

        for (scope, x) in paths {
            // When looking up an imported name, we should search the parents of the scope, but not the other imports, otherwise we'll find ourselves.
            let r = self.lookup_in_scope(scope, &x, false)?;
            results.extend(r.iter().cloned());
        }

        // FIXME: this only works when scope is a lookup scope.
        for (scope, x) in include_self {
            match scope {
                Scope::Lookup(LookupIndex(index)) => {
                    let r = self.graph.lookups.get(index).unwrap();
                    let decls = self.do_lookup(r.clone())?;
                    let found_here: Vec<Located<Decl>> = decls.iter().cloned().filter(|decl| decl.name() == x).collect();
                    results.extend(found_here.iter().cloned());
                },
                Scope::Mixfix(MixfixIndex(index)) => {
                    let r = self.graph.mixfixes.get(index).unwrap();
                    let trees = self.do_mixfix(r.clone())?;
                    let decls: Vec<Located<Decl>> = trees.iter().flat_map(|t| Namer::mixfix_tree_to_decls(t)).collect();
                    let found_here: Vec<Located<Decl>> = decls.iter().cloned().filter(|decl| decl.name() == x).collect();
                    results.extend(found_here.iter().cloned());
                },
                _ => {},
            }
        }

        self.driver.stats.accum("lookup_in_imports size", results.len() as u64);

        Ok(results)
    }

    // This resolves to the declarations in the leftmost name in
    // the mixfix tree. That is to resolve the scope of (List (Maybe Int)),
    // we just look at `List _`.
    fn mixfix_tree_to_decls(t: &MixfixTree) -> Vec<Located<Decl>> {
        match t {
            MixfixTree::Apply(ref t1, ref t2) => Namer::mixfix_tree_to_decls(&**t1),
            MixfixTree::Name(ref x, ref decls) => decls.clone(),
            MixfixTree::Exp => vec![],
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn get_inner_decls(&mut self, scope: Scope, name: &Name, follow_parents: bool, decls: &Vec<Located<Decl>>) -> LookupResult<Vec<Located<Decl>>> {
        let frames = decls.iter().flat_map(|d| {
            match d {
                Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                _ => vec![]
            }
        });
        let mut inner_decls = Vec::new();
        for s in frames {
            let mut v = self.lookup_in_scope(s, name, follow_parents)?;
            inner_decls.append(&mut v);
        }
        Ok(inner_decls)
    }

    // HACK
    // Version of lookup_here_in_scope that only works on Env environments.
    // Called from lookup_in_trees. Avoids loading other bundles which requires
    // borrowing the driver mutably. The assumption is that the root scope of a bundle
    // is always a simple Env.
    #[cfg_attr(debug_assertions, trace(disable(stats, graph)))]
    fn lookup_here_in_env(stats: &mut driver::stats::Stats, graph: &ScopeGraph, env_index: EnvIndex, name: &Name) -> LookupResult<Vec<Located<Decl>>> {
        match env_index {
            EnvIndex(index) => {
                let env = graph.envs.get(index).unwrap();
                // need to clone the includes
                // before we borrow self mutably.
                let includes = env.includes.clone();
                let found_here: Vec<Located<Decl>> = env.decls.iter().cloned().filter(|decl| decl.name() == *name).collect();

                let mut results = found_here;

                for include in includes {
                    match include {
                        Scope::Env(include) if include != env_index => {
                            let v = Namer::lookup_here_in_env(stats, graph, include, name)?;
                            results.extend(v.iter().cloned());
                        },
                        Scope::EnvWithoutImports(include) if include != env_index => {
                            let v = Namer::lookup_here_in_env(stats, graph, include, name)?;
                            results.extend(v.iter().cloned());
                        },
                        _ => {},
                    }
                }

                Ok(results)
            },
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn lookup_in_scope(&mut self, scope: Scope, name: &Name, follow_parents: bool) -> LookupResult<Vec<Located<Decl>>> {
        match self.cache.lookup_cache.get(&LookupRef { scope, name: name.clone(), follow_parents }) {
            Some(decls) => {
                println!("lookup cache hit");
                self.driver.stats.accum("lookup cache hit size", decls.len() as u64);
                return Ok(decls.clone());
            },
            None => {
                self.driver.stats.accum("lookup cache miss", 1);
                println!("lookup cache miss");
            },
        }

        // Cache the empty vec.
        // If we hit a cycle, we'll return that rather than loop forever.
        // FIXME: consider caching a Result to differentiate an empth Ok from a cycle.
        self.cache.lookup_cache.insert(LookupRef { scope, name: name.clone(), follow_parents }, vec![]);

        let result = match scope {
            Scope::Empty => Ok(vec![]),
            Scope::Global => self.lookup_in_root(name),
            Scope::Lookup(LookupIndex(index)) => {
                let r = self.graph.lookups.get(index).unwrap();
                let decls = self.do_lookup(r.clone())?;
                self.get_inner_decls(scope, &name, follow_parents, &decls)
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let r = self.graph.mixfixes.get(index).unwrap();
                let trees = self.do_mixfix(r.clone())?;
                let decls = trees.iter().flat_map(|t| Namer::mixfix_tree_to_decls(t)).collect();
                self.get_inner_decls(scope, &name, follow_parents, &decls)
            },
            Scope::EnvWithoutImports(EnvIndex(index)) => {
                let env = self.graph.envs.get(index).unwrap();
                // need to clone the imports and parents and includes
                // before we borrow self mutably.
                let imports = env.imports.clone();
                let parents = env.parents.clone();
                let includes = env.includes.clone();
                let found_here: Vec<Located<Decl>> = env.decls.iter().cloned().filter(|decl| decl.name() == *name).collect();

                let mut results = Vec::new();
                results.append(&mut found_here.clone());

                if follow_parents && Namer::all_mixfix(&results) {
                    let mut ps = Vec::new();
                    for parent in parents {
                        if self.imports_truncated(&imports, parent) {
                            continue;
                        }
                        let v = self.lookup_in_scope(parent, name, follow_parents)?;
                        ps.extend(v.iter().cloned());
                    }

                    let found_parents = Namer::filter_mixfix(ps, &results);
                    results.extend(found_parents.iter().cloned());
                }

                for include in includes {
                    let v = self.lookup_in_scope(include, name, follow_parents)?;
                    results.extend(v.iter().cloned());
                }

                Ok(results)
            }
            Scope::Env(EnvIndex(index)) => {
                let env = self.graph.envs.get(index).unwrap();
                // need to clone the imports and parents and includes
                // before we borrow self mutably.
                let imports = env.imports.clone();
                let parents = env.parents.clone();
                let includes = env.includes.clone();
                let found_here: Vec<Located<Decl>> = env.decls.iter().cloned().filter(|decl| decl.name() == *name).collect();

                let mut results = Vec::new();
                results.append(&mut found_here.clone());

                if Namer::all_mixfix(&results) {
                    let rs = self.lookup_in_imports(&imports, name)?;
                    let found_imports = Namer::filter_mixfix(rs, &results);
                    results.extend(found_imports.iter().cloned());
                }


                if follow_parents && Namer::all_mixfix(&results) {
                    let mut ps = Vec::new();
                    for parent in parents {
                        if self.imports_truncated(&imports, parent) {
                            continue;
                        }
                        let v = self.lookup_in_scope(parent, name, follow_parents)?;
                        ps.extend(v.iter().cloned());
                    }

                    let found_parents = Namer::filter_mixfix(ps, &results);
                    results.extend(found_parents.iter().cloned());
                }

                for include in includes {
                    let v = self.lookup_in_scope(include, name, follow_parents)?;
                    results.extend(v.iter().cloned());
                }

                Ok(results)
            }
        };

        match &result {
            Ok(decls) => {
                self.cache.lookup_cache.insert(LookupRef { scope, name: name.clone(), follow_parents }, decls.clone());
                self.driver.stats.accum("lookup result size", decls.len() as u64);
            },
            _ => {
                self.driver.stats.accum("lookup error", 1);
            },
        }

        result
    }

    fn imports_truncated(&self, imports: &Vec<Located<Import>>, scope: Scope) -> bool {
        for import in imports {
            match import {
                Located { loc, value: Import::None { path } } =>
                    if *path == scope {
                        return true
                    }
                _ => {},
            }
        }
        false
    }

    pub fn all_mixfix(decls: &Vec<Located<Decl>>) -> bool {
        decls.iter().all(|d| {
            match d {
                Located { loc: _, value: Decl::MixfixPart { .. } } => true,
                _ => false,
            }
        })
    }

    fn filter_mixfix(xs: Vec<Located<Decl>>, ys: &Vec<Located<Decl>>) -> Vec<Located<Decl>> {
        xs.iter().filter(|x| {
            match x {
                Located { loc: _, value: Decl::MixfixPart { full, .. } } =>
                    ys.iter().all(|y| {
                        match y {
                            Located { loc: _, value: Decl::MixfixPart { full: fully, .. } } => *full != *fully,
                            _ => true,
                        }
                    }),
                _ => true,
            }
        }).cloned().collect()
    }
}
