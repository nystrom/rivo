// ScopeGraph maintains the scope graph.

use syntax::loc::*;
use syntax::names::*;
use syntax::trees;

use namer::symbols::*;
use namer::glr;

use driver;
use driver::*;
use driver::bundle::*;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use rpds::HashTrieSet;

#[allow(non_upper_case_globals)]
static mut depth: u32 = 0;

type Crumbs = HashTrieSet<Scope>;

// The vectors in this data structure are indexed by the *Index types.

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LookupIndex(pub(super) usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LookupHereIndex(pub(super) usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MixfixIndex(pub(super) usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnvIndex(pub usize);

#[derive(Clone, Debug)]
pub struct ScopeGraph {
    // These three lists act as a worklist of lookups to preform
    // during name checking. But they are also used for ref scopes.
    lookups: Vec<LookupRef>,
    lookups_here: Vec<LookupHereRef>,
    mixfixes: Vec<MixfixRef>,

    // This is the vector of environments (mutable scopes).
    envs: Vec<Env>,
}

impl ScopeGraph {
    pub fn new() -> ScopeGraph {
        ScopeGraph {
            lookups: Vec::new(),
            lookups_here: Vec::new(),
            mixfixes: Vec::new(),
            envs: Vec::new(),
        }
    }

    pub fn new_env(&mut self) -> Scope {
        let index = self.envs.len();
        self.envs.push(Env {
            decls: Vec::new(),
            imports: Vec::new(),
            parents: Vec::new(),
            includes: Vec::new(),
        });
        Scope::Env(EnvIndex(index))
    }

    // FIXME avoid the clone here
    pub fn get_envs(&self) -> Vec<Env> {
        self.envs.clone()
    }

    pub fn get_scope_of_lookup_here(&self, r: LookupHereIndex) -> Scope {
        Scope::LookupHere(r)
    }
    pub fn get_scope_of_lookup(&self, r: LookupIndex) -> Scope {
        Scope::Lookup(r)
    }
    pub fn get_scope_of_mixfix(&self, r: MixfixIndex) -> Scope {
        Scope::Mixfix(r)
    }

    pub fn declare(&mut self, scope: Scope, decl: &Located<Decl>) {
        match scope {
            Scope::Env(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.decls.push(decl.clone())
                }
            },
            _ => {},
        }
    }
    pub fn set_parent(&mut self, scope: Scope, parent: Scope) {
        match scope {
            Scope::Env(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.parents.push(parent)
                }
            },
            _ => {},
        }
    }
    pub fn import(&mut self, scope: Scope, import: &Located<Import>) {
        match scope {
            Scope::Env(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.imports.push(import.clone())
                }
            },
            _ => {},
        }
    }
    pub fn include(&mut self, scope: Scope, include: Scope) {
        match scope {
            Scope::Env(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.includes.push(include)
                }
            },
            _ => {},
        }
    }
    pub fn lookup(&mut self, scope: Scope, name: Name) -> LookupIndex {
        let index = self.lookups.len();
        let r = LookupRef { scope: scope, name };
        self.lookups.push(r);
        LookupIndex(index)
    }
    pub fn lookup_here(&mut self, scope: Scope, name: Name) -> LookupHereIndex {
        let index = self.lookups_here.len();
        let r = LookupHereRef { scope: scope, name };
        self.lookups_here.push(r);
        LookupHereIndex(index)
    }
    pub fn parse_mixfix(&mut self, parts: Vec<MixfixPart>) -> MixfixIndex {
        let index = self.mixfixes.len();
        let r = MixfixRef { parts };
        self.mixfixes.push(r);
        MixfixIndex(index)
    }
    pub fn select_frame(&mut self, scope: Scope, name: Name) -> Scope {
        let r = self.lookup_here(scope, name);
        self.get_scope_of_lookup_here(r)
    }
    pub fn new_child_scope(&mut self, parent: Scope) -> Scope {
        let env = self.new_env();
        self.set_parent(env, parent);
        env
    }
}

// Same as Cache, but indexed by index rather than ref.
#[derive(Debug, Clone)]
pub struct SolverResult {
    pub lookup_here_cache: HashMap<LookupHereIndex, Vec<Located<Decl>>>,
    pub lookup_cache: HashMap<LookupIndex, Vec<Located<Decl>>>,
    pub mixfix_cache: HashMap<MixfixIndex, Vec<MixfixTree>>
}

// internal cache
#[derive(Debug)]
struct Cache<'a> {
    pub lookup_here_cache: &'a HashMap<LookupHereRef, Vec<Located<Decl>>>,
    pub lookup_cache: &'a HashMap<LookupRef, Vec<Located<Decl>>>,
    pub mixfix_cache: &'a HashMap<MixfixRef, Vec<MixfixTree>>
}

type LookupResult<T> = Result<T, Located<String>>;

// TODO: use this instead of the impl below
pub struct Solver<'a> {
    graph: &'a mut ScopeGraph,
    driver: &'a mut driver::Driver,
    cache: &'a mut Cache<'a>,
}

impl ScopeGraph {
    pub fn solve(&self, driver: &mut driver::Driver) -> LookupResult<SolverResult> {
        // internal cache
        let mut lookup_here_cache = HashMap::new();
        let mut lookup_cache = HashMap::new();
        let mut mixfix_cache = HashMap::new();

        // lookup result
        let mut lookup_here_cachex = HashMap::new();
        let mut lookup_cachex = HashMap::new();
        let mut mixfix_cachex = HashMap::new();

        for (i, r) in self.lookups_here.clone().iter().enumerate() {
            match lookup_here_cache.get(r) {
                None => {
                    println!("lookup_here miss");
                    let crumbs = HashTrieSet::new();
                    let cache = Cache { lookup_here_cache: &lookup_here_cache, lookup_cache: &lookup_cache, mixfix_cache: &mixfix_cache };
                    let decls = self.do_lookup_here(driver, r.clone(), &crumbs, &cache)?;
                    lookup_here_cache.insert(r.clone(), decls.clone());
                    lookup_here_cachex.insert(LookupHereIndex(i), decls);
                },
                Some(decls) => {
                    println!("lookup_here hit");
                    lookup_here_cachex.insert(LookupHereIndex(i), decls.clone());
                },
            }
        }
        for (i, r) in self.lookups.clone().iter().enumerate() {
            match lookup_cache.get(&r) {
                None => {
                    println!("lookup miss");
                    let crumbs = HashTrieSet::new();
                    let cache = Cache { lookup_here_cache: &lookup_here_cache, lookup_cache: &lookup_cache, mixfix_cache: &mixfix_cache };
                    let decls = self.do_lookup(driver, r.clone(), &crumbs, &cache)?;
                    lookup_cache.insert(r.clone(), decls.clone());
                    lookup_cachex.insert(LookupIndex(i), decls);
                },
                Some(decls) => {
                    println!("lookup hit");
                    lookup_cachex.insert(LookupIndex(i), decls.clone());
                },
            }
        }
        for (i, r) in self.mixfixes.clone().iter().enumerate() {
            match mixfix_cache.get(&r) {
                None => {
                    println!("mixfix miss");
                    let crumbs = HashTrieSet::new();
                    let cache = Cache { lookup_here_cache: &lookup_here_cache, lookup_cache: &lookup_cache, mixfix_cache: &mixfix_cache };
                    let trees = self.do_mixfix(driver, r.clone(), &crumbs, &cache)?;
                    mixfix_cache.insert(r.clone(), trees.clone());
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

    fn do_lookup(&self, driver: &mut driver::Driver, r: LookupRef, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupRef { scope, name } => self.lookup_in_scope(driver, scope, &name, crumbs, cache)
        }
    }

    fn do_lookup_here(&self, driver: &mut driver::Driver, r: LookupHereRef, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupHereRef { scope, name } => self.lookup_here_in_scope(driver, scope, &name, crumbs, cache)
        }
    }

    fn do_mixfix(&self, driver: &mut driver::Driver, r: MixfixRef, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<MixfixTree>> {
        match r {
            MixfixRef { parts } => self.resolve_mixfix(driver, &parts, crumbs, cache)
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn resolve_mixfix(&self, driver: &mut driver::Driver, parts: &Vec<MixfixPart>, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<MixfixTree>> {
        match cache.mixfix_cache.get(&MixfixRef { parts: parts.clone() }) {
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
                    let r = self.lookups.get(index);
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
                    let decls = self.do_lookup(driver, r.clone(), &crumbs, &cache)?;

                    if ! decls.is_empty() && ScopeGraph::all_mixfix(&decls) {
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
    fn lookup_in_root(&self, driver: &mut driver::Driver, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&Scope::Global) {
            return Ok(vec![]);
        }

        // we only search other bundles for legal bundle names
        if ! name.is_bundle_name() {
            return Ok(vec![]);
        }

        let mut results = self.lookup_in_trees(driver, name, &crumbs.insert(Scope::Global))?;

        if ScopeGraph::all_mixfix(&results) {
            match self.load_bundle_by_name(driver, name) {
                Ok(index) => {
                    match driver.name_bundle(index) {
                        Ok(_) => {},
                        Err(msg) => {
                            return Err(msg)
                        },
                    }

                    // FIXME: the Cache should be part of the bundle if named.
                    let cache = Cache {
                        lookup_here_cache: &HashMap::new(),
                        lookup_cache: &HashMap::new(),
                        mixfix_cache: &HashMap::new(),
                    };

                    match driver.get_bundle(index) {
                        Some(Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let mut v = self.lookup_here_in_scope(driver, *scope, name, crumbs, &cache)?;
                                results.append(&mut v);
                            }
                            else {
                                driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                            }
                        },
                        Some(Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let mut v = self.lookup_here_in_scope(driver, *scope, name, crumbs, &cache)?;
                                results.append(&mut v);
                            }
                            else {
                                driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                            }
                        },
                        Some(Bundle::Core { root_scope, .. }) => {
                            let mut v = self.lookup_here_in_scope(driver, root_scope, name, crumbs, &cache)?;
                            results.append(&mut v);
                        },
                        _ => {},
                    }
                    Ok(results)
                },
                Err(msg) => {
                    // There is no bundle with the given name, but ignore that since it's not
                    // required to exist.
                    Ok(results)
                },
            }
        }
        else {
            Ok(results)
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(driver)))]
    fn load_bundle_by_name(&self, driver: &mut driver::Driver, name: &Name) -> LookupResult<BundleIndex> {
        driver.load_bundle_by_name(name)
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, crumbs)))]
    fn lookup_in_trees(&self, driver: &mut driver::Driver, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        let mut results = Vec::new();
        let mut to_name = Vec::new();

        // bring any loaded bundles up to the naming phase.
        // we can't do the naming in this loop because of borrowing constraints;
        // instead we copy to another vector and then do the naming.
        for (index, bundle) in driver.enumerate_bundles() {
            if Some(index) != driver.current_bundle {
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

        driver.stats.accum("lookup_in_trees", 1);

        for index in to_name {
            println!("naming bundle {:?} to find {:?}", index, name);
            match driver.name_bundle(index) {
                Ok(_) => {},
                Err(msg) => {
                    // return Err(msg)
                },
            }
        }

        // Assert that we named things?
        for (index, bundle) in driver.enumerate_bundles() {
            if Some(index) != driver.current_bundle {
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
        let stats = &mut driver.stats;

        for bundle in &driver.bundles {
            match bundle {
                Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
                    if let Some(scope) = scopes.get(&id) {
                        let mut v = graph.lookup_here_in_env(stats, *scope, name, &HashTrieSet::new())?;
                        results.append(&mut v);
                    }
                },
                Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
                    if let Some(scope) = scopes.get(&id) {
                        let mut v = graph.lookup_here_in_env(stats, *scope, name, &HashTrieSet::new())?;
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
    fn lookup_in_imports(&self, driver: &mut driver::Driver, imports: &Vec<Located<Import>>, x: &Name, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
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
            let r = self.lookup_here_in_scope(driver, scope, &x, crumbs, cache)?;
            results.append(&mut r.clone());
        }

        Ok(results)
    }

    // This resolves to the declarations in the leftmost name in
    // the mixfix tree. That is to resolve the scope of (List (Maybe Int)),
    // we just look at `List _`.
    fn mixfix_tree_to_decls(t: &MixfixTree) -> Vec<Located<Decl>> {
        match t {
            MixfixTree::Apply(ref t1, ref t2) => ScopeGraph::mixfix_tree_to_decls(&**t1),
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
    fn get_inner_decls_here(&self, driver: &mut driver::Driver, scope: Scope, name: &Name, crumbs: &Crumbs, cache: &Cache, decls: &Vec<Located<Decl>>) -> LookupResult<Vec<Located<Decl>>> {
        let frames = decls.iter().flat_map(|d| {
            match d {
                Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                _ => vec![]
            }
        });
        let mut inner_decls = Vec::new();
        for s in frames {
            let mut v = self.lookup_here_in_scope(driver, s, name, &crumbs.insert(scope), cache)?;
            inner_decls.append(&mut v);
        }
        Ok(inner_decls)
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn get_inner_decls(&self, driver: &mut driver::Driver, scope: Scope, name: &Name, crumbs: &Crumbs, cache: &Cache, decls: &Vec<Located<Decl>>) -> LookupResult<Vec<Located<Decl>>> {
        let frames = decls.iter().flat_map(|d| {
            match d {
                Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                _ => vec![]
            }
        });
        let mut inner_decls = Vec::new();
        for s in frames {
            let mut v = self.lookup_in_scope(driver, s, name, &crumbs.insert(scope), cache)?;
            inner_decls.append(&mut v);
        }
        Ok(inner_decls)
    }

    #[cfg_attr(debug_assertions, trace(disable(driver, cache)))]
    fn lookup_here_in_scope(&self, driver: &mut driver::Driver, scope: Scope, name: &Name, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&scope) {
            driver.stats.accum("lookup_here cycle", 1);
            return Ok(vec![]);
        }

        match cache.lookup_here_cache.get(&LookupHereRef { scope, name: name.clone() }) {
            Some(decls) => {
                println!("lookup_here cache hit");
                driver.stats.accum("lookup_here hit", 1);
                return Ok(decls.clone())
            },
            None => {
                driver.stats.accum("lookup_here miss", 1);
                println!("lookup_here cache miss");
            },
        }

        match scope {
            Scope::Empty => Ok(vec![]),
            Scope::Global => self.lookup_in_root(driver, name, crumbs),
            Scope::Lookup(LookupIndex(index)) => {
                let r = self.lookups.get(index).unwrap();
                let decls = self.do_lookup(driver, r.clone(), &crumbs.insert(scope), &cache)?;
                self.get_inner_decls_here(driver, scope, &name, &crumbs, &cache, &decls)
            },
            Scope::LookupHere(LookupHereIndex(index)) => {
                let r = self.lookups_here.get(index).unwrap();
                let decls = self.do_lookup_here(driver, r.clone(), &crumbs.insert(scope), &cache)?;
                self.get_inner_decls_here(driver, scope, &name, &crumbs, &cache, &decls)
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let r = self.mixfixes.get(index).unwrap();
                let trees = self.do_mixfix(driver, r.clone(), &crumbs.insert(scope), &cache)?;
                let decls = trees.iter().flat_map(|t| ScopeGraph::mixfix_tree_to_decls(t)).collect();
                self.get_inner_decls_here(driver, scope, &name, &crumbs, &cache, &decls)
            },
            Scope::Env(EnvIndex(index)) => {
                let env = self.envs.get(index).unwrap();
                // need to clone the includes
                // before we borrow self mutably.
                let includes = env.includes.clone();
                let found_here: Vec<Located<Decl>> = env.decls.iter().cloned().filter(|decl| decl.name() == *name).collect();

                let mut results = found_here;

                for include in includes {
                    if include != scope {
                        let v = self.lookup_here_in_scope(driver, include, name, &crumbs.insert(scope), &cache)?;
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
    #[cfg_attr(debug_assertions, trace(disable(stats)))]
    fn lookup_here_in_env(&self, stats: &mut driver::stats::Stats, scope: Scope, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&scope) {
            stats.accum("lookup_here cycle", 1);
            return Ok(vec![]);
        }

        match scope {
            Scope::Empty => Ok(vec![]),
            Scope::Env(EnvIndex(index)) => {
                let env = self.envs.get(index).unwrap();
                // need to clone the includes
                // before we borrow self mutably.
                let includes = env.includes.clone();
                let found_here: Vec<Located<Decl>> = env.decls.iter().cloned().filter(|decl| decl.name() == *name).collect();

                let mut results = found_here;

                for include in includes {
                    if include != scope {
                        let v = self.lookup_here_in_env(stats, include, name, &crumbs.insert(scope))?;
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
    fn lookup_in_scope(&self, driver: &mut driver::Driver, scope: Scope, name: &Name, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&scope) {
            driver.stats.accum("lookup cycle", 1);
            return Ok(vec![]);
        }

        match cache.lookup_cache.get(&LookupRef { scope, name: name.clone() }) {
            Some(decls) => {
                println!("lookup cache hit");
                driver.stats.accum("lookup hit", 1);
                return Ok(decls.clone())
            },
            None => {
                driver.stats.accum("lookup miss", 1);
                println!("lookup cache miss");
            },
        }

        match scope {
            Scope::Empty => Ok(vec![]),
            Scope::Global => self.lookup_in_root(driver, name, crumbs),
            Scope::Lookup(LookupIndex(index)) => {
                let r = self.lookups.get(index).unwrap();
                let decls = self.do_lookup(driver, r.clone(), &crumbs.insert(scope), &cache)?;
                self.get_inner_decls(driver, scope, &name, &crumbs, &cache, &decls)
            },
            Scope::LookupHere(LookupHereIndex(index)) => {
                let r = self.lookups_here.get(index).unwrap();
                let decls = self.do_lookup_here(driver, r.clone(), &crumbs.insert(scope), &cache)?;
                self.get_inner_decls(driver, scope, &name, &crumbs, &cache, &decls)
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let r = self.mixfixes.get(index).unwrap();
                let trees = self.do_mixfix(driver, r.clone(), &crumbs.insert(scope), &cache)?;
                let decls = trees.iter().flat_map(|t| ScopeGraph::mixfix_tree_to_decls(t)).collect();
                self.get_inner_decls(driver, scope, &name, &crumbs, &cache, &decls)
            },
            Scope::Env(EnvIndex(index)) => {
                let env = self.envs.get(index).unwrap();
                // need to clone the imports and parents and includes
                // before we borrow self mutably.
                let imports = env.imports.clone();
                let parents = env.parents.clone();
                let includes = env.includes.clone();
                let found_here = self.lookup_here_in_scope(driver, scope, name, crumbs, cache)?;

                let mut results = Vec::new();
                results.append(&mut found_here.clone());

                if ScopeGraph::all_mixfix(&found_here) {
                    let rs = self.lookup_in_imports(driver, &imports, name, &crumbs.insert(scope), &cache)?;
                    let found_imports = ScopeGraph::filter_mixfix(rs, &results);

                    results.extend(found_imports.iter().cloned());

                    if ScopeGraph::all_mixfix(&found_imports) {
                        let mut ps = Vec::new();
                        for parent in parents {
                            let v = self.lookup_in_scope(driver, parent, name, &crumbs.insert(scope), &cache)?;
                            ps.extend(v.iter().cloned());
                        }

                        let found_parents = ScopeGraph::filter_mixfix(ps, &results);
                        results.extend(found_parents.iter().cloned());
                    }
                }

                for include in includes {
                    let v = self.lookup_in_scope(driver, include, name, &crumbs.insert(scope), &cache)?;
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


use crate::namer::glr::*;
use crate::syntax::trees::NodeId;

struct GLRAdapter;

impl GLRAdapter {

    fn make_rhs_from_name(x: &Name, assoc: Option<usize>, prio: Prio, id: NodeId) -> Vec<Symbol> {
        let lowest = Symbol::Nonterm(Nonterm::E);
        let current = Symbol::Nonterm(Nonterm::M(id, prio.0));
        let next = Symbol::Nonterm(Nonterm::M(id, prio.0 + 1));
        let highest = Symbol::Nonterm(Nonterm::Br);

        match x {
            Name::Op(x) => {
                vec![Symbol::Term(Term::Name(Part::Op(x.clone())))]
            },
            Name::Id(x) => {
                vec![Symbol::Term(Term::Name(Part::Id(x.clone())))]
            },
            Name::Mixfix(parts) => {
                match parts.as_slice() {
                    [] => vec![],
                    [Part::Op(x), Part::Placeholder] => {
                        // Unary prefix operators are right associative.
                        vec![Symbol::Term(Term::Name(Part::Op(x.clone()))), current]
                    },
                    [Part::Placeholder, Part::Op(x)] => {
                        // Unary postfix operators are left associative.
                        vec![current, Symbol::Term(Term::Name(Part::Op(x.clone())))]
                    },
                    parts => {
                        println!("parts {:?}", parts);

                        // map all expressions to the highest precedence nonterminal.
                        let mut rhs: Vec<Symbol> = parts.iter().map(|part|
                            match part {
                                Part::Placeholder => highest.clone(),
                                x => Symbol::Term(Term::Name(x.clone())),
                            }).collect();

                        let n = rhs.len();

                        // Handle the associativity annotations on the first and last tokens.
                        if n > 2 {
                            match (&rhs[0], &rhs[1]) {
                                // _ + ...
                                (Symbol::Nonterm(_), Symbol::Term(_)) => {
                                    if assoc == Some(0) || assoc == None {
                                        rhs[0] = current.clone();
                                    }
                                    else {
                                        rhs[0] = next.clone();
                                    }
                                }
                                _ => {},
                            }

                            match (&rhs[n-2], &rhs[n-1]) {
                                // ... + _
                                (Symbol::Term(_), Symbol::Nonterm(_)) => {
                                    if assoc == Some(n-1) {
                                        rhs[n-1] = current.clone();
                                    }
                                    else {
                                        rhs[n-1] = next.clone();
                                    }
                                }
                                _ => {},
                            }
                        }

                        for i in 0..n {
                            if 1 <= i && i+1 < n {
                                match (&rhs[i-1], &rhs[i], &rhs[i+1]) {
                                    // Hole: a nonterminal surrounded by terminals has lowest priority.
                                    // if _ then _ else
                                    // | _ |
                                    (Symbol::Term(_), Symbol::Nonterm(_), Symbol::Term(_)) => {
                                        rhs[i] = lowest.clone();
                                    },
                                    _ => {},
                                }

                                match (&rhs[i-1], &rhs[i], &rhs[i+1]) {
                                    // _ _ x --> use lowest priority before x
                                    (Symbol::Nonterm(Nonterm::Br), Symbol::Nonterm(_), Symbol::Term(_)) => {
                                        rhs[i] = lowest.clone();
                                    },
                                    _ => {},
                                }
                            }
                        }

                        if n > 2 {
                            match (&rhs[n-2], &rhs[n-1]) {
                                (Symbol::Nonterm(Nonterm::Br), Symbol::Nonterm(_)) => {
                                    rhs[n-1] = lowest.clone();
                                }
                                _ => {},
                            }
                        }

                        println!("rhs {:?}", rhs);
                        rhs
                    },
                }
            },
        }
    }

    fn make_rhs_from_decl(decl: &Decl) -> Rule {
        let name = decl.name();
        let assoc = decl.assoc();
        let prio = Prio(0);
        let id = NodeId(0);
        let rhs = GLRAdapter::make_rhs_from_name(&name, assoc, prio, id);
        Rule { lhs: Nonterm::M(id, prio.0), rhs: rhs }
    }

    fn glr_from_decls(decls: &Vec<Decl>) -> glr::GLR {
        // S -> E $
        let start_rule = Rule { lhs: Nonterm::S, rhs: vec![Symbol::Nonterm(Nonterm::E), Symbol::Term(Term::End)] };
        // Pr -> Br
        let pr_br = Rule { lhs: Nonterm::Pr, rhs: vec![Symbol::Nonterm(Nonterm::Br)] };
        // Br -> p
        let br_p = Rule { lhs: Nonterm::Br, rhs: vec![Symbol::Term(Term::Primary)] };
        // E -> Pr
        let e_pr = Rule { lhs: Nonterm::E, rhs: vec![Symbol::Nonterm(Nonterm::Pr)] };
        // Pr -> Pr Br
        let pr_pr_br = Rule { lhs: Nonterm::Pr, rhs: vec![Symbol::Nonterm(Nonterm::Pr), Symbol::Nonterm(Nonterm::Br)] };

        let mut rules = vec![
            start_rule.clone(),
            pr_br,
            br_p,
            pr_pr_br,
        ];

        let decl_rules = decls.iter().map(|decl| GLRAdapter::make_rhs_from_decl(decl));
        rules.extend(decl_rules);

        let mut min_prio = HashMap::new();
        let mut max_prio = HashMap::new();
        let mut keys = HashSet::new();

        for rule in &rules {
            match rule.lhs {
                Nonterm::M(id, prio) => {
                    keys.insert(id);
                    match min_prio.get(&id) {
                        Some(i) if prio < *i => { min_prio.insert(id, prio); },
                        None => { min_prio.insert(id, prio); },
                        _ => {},
                    }
                    match max_prio.get(&id) {
                        Some(i) if prio > *i => { max_prio.insert(id, prio); },
                        None => { max_prio.insert(id, prio); },
                        _ => {},
                    }
                }
                _ => {},
            }
        }

        // Add E -> Pr if there are no other rules.
        if keys.is_empty() {
            rules.push(e_pr);
        }

        for id in keys {
            match (min_prio.get(&id), max_prio.get(&id)) {
                (Some(min), Some(max)) => {
                    // Mmin -> Mn -> M{n+1} -> ... -> Mmax
                    for k in *min .. *max {
                        rules.push(
                            Rule {
                                lhs: Nonterm::M(id, k),
                                rhs: vec![Symbol::Nonterm(Nonterm::M(id, k+1))],
                            }
                        );
                    }

                    // E -> Mmin
                    rules.push(
                        Rule {
                            lhs: Nonterm::E,
                            rhs: vec![Symbol::Nonterm(Nonterm::M(id, *min))],
                        }
                    );

                    // Mmax -> Br
                    rules.push(
                        Rule {
                            lhs: Nonterm::M(id, *max),
                            rhs: vec![Symbol::Nonterm(Nonterm::Pr)],
                        }
                    );
                },
                _ => {},
            }
        }

        println!("rules {:#?}", &rules);

        let mut lr = LR::new(rules);
        let state = lr.add_state(lr.closure(vec![Item { dot: 0, rule: start_rule }])).unwrap();
        GLR::new(lr, state)
    }
}
