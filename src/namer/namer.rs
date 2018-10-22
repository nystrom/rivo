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

// Same as Cache, but indexed by index rather than ref.
#[derive(Debug, Clone)]
pub struct SolverResult {
    pub lookup_here_cache: HashMap<LookupHereIndex, Vec<Located<Decl>>>,
    pub lookup_cache: HashMap<LookupIndex, Vec<Located<Decl>>>,
    pub mixfix_cache: HashMap<MixfixIndex, Vec<MixfixTree>>
}

// internal cache
#[derive(Debug)]
pub struct Cache<'a> {
    pub lookup_here_cache: &'a mut HashMap<LookupHereRef, Vec<Located<Decl>>>,
    pub lookup_cache: &'a mut HashMap<LookupRef, Vec<Located<Decl>>>,
    pub mixfix_cache: &'a mut HashMap<MixfixRef, Vec<MixfixTree>>
}

type LookupResult<T> = Result<T, Located<String>>;

// TODO: use this instead of the impl below
pub struct Namer<'a> {
    pub graph: &'a mut ScopeGraph,
    pub driver: &'a mut driver::Driver,
    pub cache: &'a mut Cache<'a>,
}

impl<'a> Namer<'a> {
    pub fn solve(&mut self) -> LookupResult<SolverResult> {
        // lookup result
        let mut lookup_here_cachex = HashMap::new();
        let mut lookup_cachex = HashMap::new();
        let mut mixfix_cachex = HashMap::new();

        for (i, r) in self.graph.lookups_here.clone().iter().enumerate() {
            match self.cache.lookup_here_cache.get(r) {
                None => {
                    println!("lookup_here miss");
                    let crumbs = HashTrieSet::new();
                    let decls = self.do_lookup_here(r.clone(), &crumbs)?;
                    self.cache.lookup_here_cache.insert(r.clone(), decls.clone());
                    lookup_here_cachex.insert(LookupHereIndex(i), decls);
                },
                Some(decls) => {
                    println!("lookup_here hit");
                    lookup_here_cachex.insert(LookupHereIndex(i), decls.clone());
                },
            }
        }
        for (i, r) in self.graph.lookups.clone().iter().enumerate() {
            match self.cache.lookup_cache.get(&r) {
                None => {
                    println!("lookup miss");
                    let crumbs = HashTrieSet::new();
                    let decls = self.do_lookup(r.clone(), &crumbs)?;
                    self.cache.lookup_cache.insert(r.clone(), decls.clone());
                    lookup_cachex.insert(LookupIndex(i), decls);
                },
                Some(decls) => {
                    println!("lookup hit");
                    lookup_cachex.insert(LookupIndex(i), decls.clone());
                },
            }
        }
        for (i, r) in self.graph.mixfixes.clone().iter().enumerate() {
            match self.cache.mixfix_cache.get(&r) {
                None => {
                    println!("mixfix miss");
                    let crumbs = HashTrieSet::new();
                    let trees = self.do_mixfix(r.clone(), &crumbs)?;
                    self.cache.mixfix_cache.insert(r.clone(), trees.clone());
                    mixfix_cachex.insert(MixfixIndex(i), trees);
                },
                Some(trees) => {
                    println!("mixfix hit");
                    mixfix_cachex.insert(MixfixIndex(i), trees.clone());
                },
            }
        }

        Ok(
            SolverResult {
                lookup_here_cache: lookup_here_cachex,
                lookup_cache: lookup_cachex,
                mixfix_cache: mixfix_cachex,
            }
        )
    }

    fn do_lookup(&mut self, r: LookupRef, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupRef { scope, name } => self.lookup_in_scope(scope, &name, crumbs)
        }
    }

    fn do_lookup_here(&mut self, r: LookupHereRef, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupHereRef { scope, name } => self.lookup_here_in_scope(scope, &name, crumbs)
        }
    }

    fn do_mixfix(&mut self, r: MixfixRef, crumbs: &Crumbs) -> LookupResult<Vec<MixfixTree>> {
        match r {
            MixfixRef { parts } => self.resolve_mixfix(&parts, crumbs)
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn resolve_mixfix(&mut self, parts: &Vec<MixfixPart>, crumbs: &Crumbs) -> LookupResult<Vec<MixfixTree>> {
        match self.cache.mixfix_cache.get(&MixfixRef { parts: parts.clone() }) {
            Some(trees) => {
                println!("mixfix cache hit");
                return Ok(trees.clone())
            },
            None => {
                println!("mixfix cache miss");
            },
        }

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
                    let decls = self.do_lookup(r.clone(), &crumbs)?;

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

            Ok(vec![tree])
        }
        else {
            // Resolve the mixfix expression.

            // NOTE: do not add the $ token to the input.. GLR will do that for us.
            // tokens.push(Token::End);

            let decls = tokens.iter().flat_map(|token| {
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

            let mut parser = GLRAdapter::glr_from_decls(&decls);

            Ok(parser.parse(tokens))
        }
    }


    #[cfg_attr(debug_assertions, trace(disable(driver, crumbs)))]
    fn lookup_in_root(&mut self, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&Scope::Global) {
            return Ok(vec![]);
        }

        // we only search other bundles for legal bundle names
        if ! name.is_bundle_name() {
            return Ok(vec![]);
        }

        let mut results = self.lookup_in_trees(name, &crumbs.insert(Scope::Global))?;

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
                        lookup_here_cache: &mut HashMap::new(),
                        lookup_cache: &mut HashMap::new(),
                        mixfix_cache: &mut HashMap::new(),
                    };

                    match self.driver.get_bundle(index) {
                        Some(Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let mut v = self.lookup_here_in_scope(*scope, name, crumbs)?;
                                results.append(&mut v);
                            }
                            else {
                                self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                            }
                        },
                        Some(Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let mut v = self.lookup_here_in_scope(*scope, name, crumbs)?;
                                results.append(&mut v);
                            }
                            else {
                                self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                            }
                        },
                        Some(Bundle::Core { root_scope, .. }) => {
                            let mut v = self.lookup_here_in_scope(root_scope, name, crumbs)?;
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

    #[cfg_attr(debug_assertions, trace(disable(driver, crumbs)))]
    fn lookup_in_trees(&mut self, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
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
        // We want to call lookup_here_in_scope, but can't since we have to mutably borrow driver
        // while we're looping over it.
        // We pass in empty crumbs since crumbs are bundle specific.
        let stats = &mut self.driver.stats;

        for bundle in &self.driver.bundles {
            match bundle {
                Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
                    if let Some(scope) = scopes.get(&id) {
                        let mut v = Namer::lookup_here_in_env(stats, graph, *scope, name, &HashTrieSet::new())?;
                        results.append(&mut v);
                    }
                },
                Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
                    if let Some(scope) = scopes.get(&id) {
                        let mut v = Namer::lookup_here_in_env(stats, graph, *scope, name, &HashTrieSet::new())?;
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
        //     let mut v = graph.lookup_here_in_scope(driver, *scope, name, &HashTrieSet::new(), &Cache { lookup_here_cache: &HashMap::new(), lookup_cache: &HashMap::new(), mixfix_cache: &HashMap::new() })?;
        //     results.append(&mut v);
        // }

        Ok(results)
    }

    #[cfg_attr(debug_assertions, cfg_attr(debug_assertions, trace(disable(driver, cache))))]
    fn lookup_in_imports(&mut self, imports: &Vec<Located<Import>>, x: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
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

        for (scope, x) in paths {
            let r = self.lookup_here_in_scope(scope, &x, crumbs)?;
            results.append(&mut r.clone());
        }

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

    // Return true if the given list of imports includes the current scope as import none.
    fn imports_truncate(imports: &Vec<Located<Import>>, scope: Scope) -> bool {
        for Located { loc, value } in imports {
            match value {
                Import::None { path: s } if *s == scope => return true,
                _ => {},
            }
        }
        false
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn get_inner_decls_here(&mut self, scope: Scope, name: &Name, crumbs: &Crumbs, decls: &Vec<Located<Decl>>) -> LookupResult<Vec<Located<Decl>>> {
        let frames = decls.iter().flat_map(|d| {
            match d {
                Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                _ => vec![]
            }
        });
        let mut inner_decls = Vec::new();
        for s in frames {
            let mut v = self.lookup_here_in_scope(s, name, &crumbs.insert(scope))?;
            inner_decls.append(&mut v);
        }
        Ok(inner_decls)
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn get_inner_decls(&mut self, scope: Scope, name: &Name, crumbs: &Crumbs, decls: &Vec<Located<Decl>>) -> LookupResult<Vec<Located<Decl>>> {
        let frames = decls.iter().flat_map(|d| {
            match d {
                Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                _ => vec![]
            }
        });
        let mut inner_decls = Vec::new();
        for s in frames {
            let mut v = self.lookup_in_scope(s, name, &crumbs.insert(scope))?;
            inner_decls.append(&mut v);
        }
        Ok(inner_decls)
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn lookup_here_in_scope(&mut self, scope: Scope, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&scope) {
            self.driver.stats.accum("lookup_here cycle", 1);
            return Ok(vec![]);
        }

        match self.cache.lookup_here_cache.get(&LookupHereRef { scope, name: name.clone() }) {
            Some(decls) => {
                println!("lookup_here cache hit");
                self.driver.stats.accum("lookup_here hit", 1);
                return Ok(decls.clone())
            },
            None => {
                self.driver.stats.accum("lookup_here miss", 1);
                println!("lookup_here cache miss");
            },
        }

        match scope {
            Scope::Empty => Ok(vec![]),
            Scope::Global => self.lookup_in_root(name, crumbs),
            Scope::Lookup(LookupIndex(index)) => {
                let r = self.graph.lookups.get(index).unwrap();
                let decls = self.do_lookup(r.clone(), &crumbs.insert(scope))?;
                self.get_inner_decls_here(scope, &name, &crumbs, &decls)
            },
            Scope::LookupHere(LookupHereIndex(index)) => {
                let r = self.graph.lookups_here.get(index).unwrap();
                let decls = self.do_lookup_here(r.clone(), &crumbs.insert(scope))?;
                self.get_inner_decls_here(scope, &name, &crumbs, &decls)
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let r = self.graph.mixfixes.get(index).unwrap();
                let trees = self.do_mixfix(r.clone(), &crumbs.insert(scope))?;
                let decls = trees.iter().flat_map(|t| Namer::mixfix_tree_to_decls(t)).collect();
                self.get_inner_decls_here(scope, &name, &crumbs, &decls)
            },
            Scope::Env(EnvIndex(index)) => {
                let env = self.graph.envs.get(index).unwrap();
                // need to clone the includes
                // before we borrow self mutably.
                let includes = env.includes.clone();
                let found_here: Vec<Located<Decl>> = env.decls.iter().cloned().filter(|decl| decl.name() == *name).collect();

                let mut results = found_here;

                for include in includes {
                    if include != scope {
                        let v = self.lookup_here_in_scope(include, name, &crumbs.insert(scope))?;
                        results.extend(v.iter().cloned());
                    }
                }

                Ok(results)
            }
        }
    }

    // HACK
    // Version of lookup_here_in_scope that only works on Env environments.
    // Called from lookup_in_trees. Avoids loading other bundles which requires
    // borrowing the driver mutably. The assumption is that the root scope of a bundle
    // is always a simple Env.
    #[cfg_attr(debug_assertions, trace(disable(stats, graph)))]
    fn lookup_here_in_env(stats: &mut driver::stats::Stats, graph: &ScopeGraph, scope: Scope, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&scope) {
            stats.accum("lookup_here cycle", 1);
            return Ok(vec![]);
        }

        match scope {
            Scope::Empty => Ok(vec![]),
            Scope::Env(EnvIndex(index)) => {
                let env = graph.envs.get(index).unwrap();
                // need to clone the includes
                // before we borrow self mutably.
                let includes = env.includes.clone();
                let found_here: Vec<Located<Decl>> = env.decls.iter().cloned().filter(|decl| decl.name() == *name).collect();

                let mut results = found_here;

                for include in includes {
                    if include != scope {
                        let v = Namer::lookup_here_in_env(stats, graph, include, name, &crumbs.insert(scope))?;
                        results.extend(v.iter().cloned());
                    }
                }

                Ok(results)
            },
            _ => {
                Ok(vec![])
            }
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn lookup_in_scope(&mut self, scope: Scope, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&scope) {
            self.driver.stats.accum("lookup cycle", 1);
            return Ok(vec![]);
        }

        match self.cache.lookup_cache.get(&LookupRef { scope, name: name.clone() }) {
            Some(decls) => {
                println!("lookup cache hit");
                self.driver.stats.accum("lookup hit", 1);
                return Ok(decls.clone())
            },
            None => {
                self.driver.stats.accum("lookup miss", 1);
                println!("lookup cache miss");
            },
        }

        match scope {
            Scope::Empty => Ok(vec![]),
            Scope::Global => self.lookup_in_root(name, crumbs),
            Scope::Lookup(LookupIndex(index)) => {
                let r = self.graph.lookups.get(index).unwrap();
                let decls = self.do_lookup(r.clone(), &crumbs.insert(scope))?;
                self.get_inner_decls(scope, &name, &crumbs, &decls)
            },
            Scope::LookupHere(LookupHereIndex(index)) => {
                let r = self.graph.lookups_here.get(index).unwrap();
                let decls = self.do_lookup_here(r.clone(), &crumbs.insert(scope))?;
                self.get_inner_decls(scope, &name, &crumbs, &decls)
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let r = self.graph.mixfixes.get(index).unwrap();
                let trees = self.do_mixfix(r.clone(), &crumbs.insert(scope))?;
                let decls = trees.iter().flat_map(|t| Namer::mixfix_tree_to_decls(t)).collect();
                self.get_inner_decls(scope, &name, &crumbs, &decls)
            },
            Scope::Env(EnvIndex(index)) => {
                let env = self.graph.envs.get(index).unwrap();
                // need to clone the imports and parents and includes
                // before we borrow self mutably.
                let imports = env.imports.clone();
                let parents = env.parents.clone();
                let includes = env.includes.clone();
                let found_here = self.lookup_here_in_scope(scope, name, crumbs)?;

                let mut results = Vec::new();
                results.append(&mut found_here.clone());

                if Namer::all_mixfix(&found_here) {
                    let rs = self.lookup_in_imports(&imports, name, &crumbs.insert(scope))?;
                    let found_imports = Namer::filter_mixfix(rs, &results);

                    results.extend(found_imports.iter().cloned());

                    if Namer::all_mixfix(&found_imports) {
                        let mut ps = Vec::new();
                        for parent in parents {
                            let v = self.lookup_in_scope(parent, name, &crumbs.insert(scope))?;
                            ps.extend(v.iter().cloned());
                        }

                        let found_parents = Namer::filter_mixfix(ps, &results);
                        results.extend(found_parents.iter().cloned());
                    }
                }

                for include in includes {
                    let v = self.lookup_in_scope(include, name, &crumbs.insert(scope))?;
                    results.extend(v.iter().cloned());
                }

                Ok(results)
            }
        }
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
