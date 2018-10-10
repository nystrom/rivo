// ScopeGraph maintains the scope graph.

use syntax::loc::*;
use syntax::names::*;
use syntax::trees;
use namer::symbols::*;
use driver;
use driver::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use rpds::HashTrieSet;

#[allow(non_upper_case_globals)]
static mut depth: u32 = 0;

type Crumbs = HashTrieSet<Scope>;

// The vectors in this data structure are indexed by the *Index types.

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LookupIndex(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LookupHereIndex(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MixfixIndex(usize);

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

#[derive(Debug)]
pub struct Cachex {
    pub lookup_here_cache: HashMap<LookupHereRef, Vec<Located<Decl>>>,
    pub lookup_cache: HashMap<LookupRef, Vec<Located<Decl>>>,
    pub mixfix_cache: HashMap<MixfixRef, Vec<Located<MixfixTree>>>
}

// FIXME: is there a way to derive this type from the above type?
#[derive(Debug)]
pub struct Cache<'a> {
    pub lookup_here_cache: &'a HashMap<LookupHereRef, Vec<Located<Decl>>>,
    pub lookup_cache: &'a HashMap<LookupRef, Vec<Located<Decl>>>,
    pub mixfix_cache: &'a HashMap<MixfixRef, Vec<Located<MixfixTree>>>
}

type LookupResult<T> = Result<T, Located<String>>;

// TODO: use this instead of the impl below
pub struct Solver<'a> {
    graph: &'a mut ScopeGraph,
    driver: &'a mut driver::Interpreter,
    cache: &'a mut Cache<'a>,
}

impl ScopeGraph {
    // The only effect this has is to update the cache.
    // TODO:
    // Make the actual lookup take an immutable &self,
    // but pass the cache around as mutable.
    pub fn solve(&mut self, driver: &mut driver::Interpreter) -> LookupResult<Cachex> {
        let mut lookup_here_cache = HashMap::new();
        let mut lookup_cache = HashMap::new();
        let mut mixfix_cache = HashMap::new();

        for r in self.lookups_here.clone() {
            match lookup_here_cache.get(&r) {
                None => {
                    println!("lookup_here miss");
                    let crumbs = HashTrieSet::new();
                    let cache = Cache { lookup_here_cache: &lookup_here_cache, lookup_cache: &lookup_cache, mixfix_cache: &mixfix_cache };
                    let decls = self.do_lookup_here(driver, r.clone(), &crumbs, &cache)?;
                    lookup_here_cache.insert(r, decls);
                },
                Some(decls) => {
                    println!("lookup_here hit");
                },
            }
        }
        for r in self.lookups.clone() {
            match lookup_cache.get(&r) {
                None => {
                    println!("lookup miss");
                    let crumbs = HashTrieSet::new();
                    let cache = Cache { lookup_here_cache: &lookup_here_cache, lookup_cache: &lookup_cache, mixfix_cache: &mixfix_cache };
                    let decls = self.do_lookup(driver, r.clone(), &crumbs, &cache)?;
                    lookup_cache.insert(r, decls);
                },
                Some(decls) => {
                    println!("lookup hit");
                },
            }
        }
        for r in self.mixfixes.clone() {
            match mixfix_cache.get(&r) {
                None => {
                    println!("mixfix miss");
                    let crumbs = HashTrieSet::new();
                    let cache = Cache { lookup_here_cache: &lookup_here_cache, lookup_cache: &lookup_cache, mixfix_cache: &mixfix_cache };
                    let trees = self.do_mixfix(driver, r.clone(), &crumbs, &cache)?;
                    mixfix_cache.insert(r, trees);
                },
                Some(trees) => {
                    println!("mixfix hit");
                },
            }
        }

        Ok(
            Cachex {
                lookup_here_cache,
                lookup_cache,
                mixfix_cache,
            }
        )
    }

    fn do_lookup(&mut self, driver: &mut driver::Interpreter, r: LookupRef, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupRef { scope, name } => self.lookup_in_scope(driver, scope, &name, crumbs, cache)
        }
    }

    fn do_lookup_here(&mut self, driver: &mut driver::Interpreter, r: LookupHereRef, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupHereRef { scope, name } => self.lookup_here_in_scope(driver, scope, &name, crumbs, cache)
        }
    }

    fn do_mixfix(&mut self, driver: &mut driver::Interpreter, r: MixfixRef, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<MixfixTree>>> {
        match r {
            MixfixRef { parts } => self.resolve_mixfix(driver, &parts, crumbs, cache)
        }
    }

    #[trace(disable(driver, cache))]
    fn resolve_mixfix(&mut self, driver: &mut driver::Interpreter, parts: &Vec<MixfixPart>, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<MixfixTree>>> {
        match cache.mixfix_cache.get(&MixfixRef { parts: parts.clone() }) {
            Some(trees) => {
                println!("mixfix cache hit");
                return Ok(trees.clone())
            },
            None => {
                println!("mixfix cache miss");
            },
        }

        // TODO

        // make m_tokens
        // lookup the decls to make the grammar
        // call glr with the grammar and the input

        Ok(vec![])
    }

    #[trace(disable(driver, crumbs))]
    fn lookup_in_root(&mut self, driver: &mut driver::Interpreter, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
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
                    match driver.read_bundle(index) {
                        Ok(_) => {
                            match driver.name_bundle(index) {
                                Ok(_) => {},
                                Err(msg) => {
                                    return Err(msg)
                                },
                            }
                        },
                        Err(msg) => {
                            // If we can't read the bundle, ignore it... it just means
                            // the file doesn't exist.
                            // If we can't name the bundle, after reading it, then error.
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
                        },
                        Some(Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let mut v = self.lookup_here_in_scope(driver, *scope, name, crumbs, &cache)?;
                                results.append(&mut v);
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
                    // Err(msg)
                    Ok(results)
                },
            }
        }
        else {
            Ok(results)
        }
    }

    #[trace(disable(driver))]
    fn load_bundle_by_name(&mut self, driver: &mut driver::Interpreter, name: &Name) -> LookupResult<BundleIndex> {
        driver.load_bundle_by_name(name)
    }

    #[trace(disable(driver, crumbs))]
    fn lookup_in_trees(&mut self, driver: &mut driver::Interpreter, name: &Name, crumbs: &Crumbs) -> LookupResult<Vec<Located<Decl>>> {
        let mut results = Vec::new();

        let mut to_name = Vec::new();

        // bring any loaded bundles up to the naming phase.
        // we can't do the naming in this loop because of borrowing constraints;
        // instead we copy to another vector and then do the naming.
        for (index, bundle) in driver.enumerate_bundles() {
            if Some(index) != driver.current_bundle {
                match bundle {
                    Bundle::New { .. } => {
                        to_name.push(index);
                    },
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

        for index in to_name {
            println!("naming bundle {:?} to find {:?}", index, name);
            match driver.name_bundle(index) {
                Ok(_) => {},
                Err(msg) => {
                    // return Err(msg)
                },
            }
        }

        let cache = Cache {
            lookup_here_cache: &HashMap::new(),
            lookup_cache: &HashMap::new(),
            mixfix_cache: &HashMap::new(),
        };

        // search all the named bundles
        // OUCH: cloning the bundles is expensive.
        // FIXME
        for bundle in driver.bundles.clone() {
            match bundle {
                Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. } => {
                    if let Some(scope) = scopes.get(&id) {
                        let mut v = self.lookup_here_in_scope(driver, *scope, name, crumbs, &cache)?;
                        results.append(&mut v);
                    }
                },
                Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. } => {
                    if let Some(scope) = scopes.get(&id) {
                        let mut v = self.lookup_here_in_scope(driver, *scope, name, crumbs, &cache)?;
                        results.append(&mut v);
                    }
                },
                Bundle::Core { root_scope, .. } => {
                    let mut v = self.lookup_here_in_scope(driver, root_scope, name, crumbs, &cache)?;
                    results.append(&mut v);
                },
                _ => {},
            }
        }

        Ok(results)
    }

    #[trace(disable(driver, cache))]
    fn lookup_in_imports(&mut self, driver: &mut driver::Interpreter, imports: &Vec<Located<Import>>, x: &Name, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
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

    #[trace(disable(driver, cache))]
    fn get_inner_decls_here(&mut self, driver: &mut driver::Interpreter, scope: Scope, name: &Name, crumbs: &Crumbs, cache: &Cache, decls: &Vec<Located<Decl>>) -> LookupResult<Vec<Located<Decl>>> {
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

    #[trace(disable(driver, cache))]
    fn get_inner_decls(&mut self, driver: &mut driver::Interpreter, scope: Scope, name: &Name, crumbs: &Crumbs, cache: &Cache, decls: &Vec<Located<Decl>>) -> LookupResult<Vec<Located<Decl>>> {
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

    #[trace(disable(driver, cache))]
    fn lookup_here_in_scope(&mut self, driver: &mut driver::Interpreter, scope: Scope, name: &Name, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&scope) {
            return Ok(vec![]);
        }

        match cache.lookup_here_cache.get(&LookupHereRef { scope, name: name.clone() }) {
            Some(decls) => {
                println!("lookup_here cache hit");
                return Ok(decls.clone())
            },
            None => {
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
                let decls = trees.iter().flat_map(|Located { ref loc, value: ref t }| ScopeGraph::mixfix_tree_to_decls(t)).collect();
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

    #[trace(disable(driver, cache))]
    fn lookup_in_scope(&mut self, driver: &mut driver::Interpreter, scope: Scope, name: &Name, crumbs: &Crumbs, cache: &Cache) -> LookupResult<Vec<Located<Decl>>> {
        if crumbs.contains(&scope) {
            return Ok(vec![]);
        }

        match cache.lookup_cache.get(&LookupRef { scope, name: name.clone() }) {
            Some(decls) => {
                println!("lookup cache hit");
                return Ok(decls.clone())
            },
            None => {
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
                let decls = trees.iter().flat_map(|Located { ref loc, value: ref t }| ScopeGraph::mixfix_tree_to_decls(t)).collect();
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

    fn all_mixfix(decls: &Vec<Located<Decl>>) -> bool {
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
