// ScopeGraph maintains the scope graph.

use syntax::loc::*;
use syntax::names::*;
use syntax::trees;
use namer::symbols::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use rpds::HashTrieSet;

type Crumbs = HashTrieSet<Scope>;

// The vectors in this data structure are indexed by the *Index types.

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LookupIndex(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LookupHereIndex(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MixfixIndex(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnvIndex(usize);

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

    pub fn get_scope_of_lookup_here(&self, r: LookupHereIndex) -> Scope {
        Scope::LookupHere(r)
    }
    pub fn get_scope_of_lookup(&self, r: LookupIndex) -> Scope {
        Scope::Lookup(r)
    }
    pub fn get_scope_of_mixfix(&self, r: MixfixIndex) -> Scope {
        Scope::Mixfix(r)
    }
    pub fn get_empty_scope(&self) -> Scope {
        Scope::Empty
    }
    pub fn get_global_scope(&self) -> Scope {
        Scope::Global
    }

    pub fn declare(&mut self, scope: Scope, decl: Decl) {
        match scope {
            Scope::Env(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.decls.push(decl)
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

    // The only effect this has is to update the cache.
    // TODO:
    // Make the actual lookup take an immutable &self,
    // but pass the cache around as mutable.
    pub fn solve(&mut self) {
        // for r in self.lookups_here {
        //     self.do_lookup_here(r, &HashTrieSet::new());
        // }
        // for r in self.lookups {
        //     self.do_lookup(r, &HashTrieSet::new());
        // }
        // for r in self.mixfixes {
        //     self.do_mixfix(r, &HashTrieSet::new());
        // }
    }

    fn do_lookup(&mut self, r: LookupRef, crumbs: &HashTrieSet<Scope>) -> Vec<Located<Decl>> {
        match r {
            LookupRef { scope, name } => self.lookup_in_scope(scope, &name, crumbs)
        }
        // FIXME: if crumbs.is_empty() cache!
    }

    fn do_lookup_here(&mut self, r: LookupHereRef, crumbs: &HashTrieSet<Scope>) -> Vec<Located<Decl>> {
        match r {
            LookupHereRef { scope, name } => self.lookup_here_in_scope(scope, &name, crumbs)
        }

        // FIXME: if crumbs.is_empty() cache!
    }

    fn do_mixfix(&mut self, r: MixfixRef, crumbs: &HashTrieSet<Scope>) -> Vec<Located<MixfixTree>> {
        match r {
            MixfixRef { parts } => self.resolve_mixfix(&parts, crumbs)
        }
        // FIXME: if crumbs.is_empty() cache!
    }

    fn resolve_mixfix(&mut self, parts: &Vec<MixfixPart>, crumbs: &HashTrieSet<Scope>) -> Vec<Located<MixfixTree>> {
        unimplemented!();
        // make m_tokens
        // lookup the decls to make the grammar
        // call glr with the grammar and the input

    }

    fn get_loaded_trees() -> Vec<trees::Root> {
        // get namer results stored in the interpreter state
        unimplemented!()
    }

    fn load_from_bundle(name: &Name) -> Option<trees::Root> {
        // load the given bundle and run the prenamer on it to create the scopes
        // so that lookup_in_trees will find it.

        // FIXME: consider storing all the scoping information separately.
        // The bundle saved in the interpreter state needs to save the namer
        // results anyway (including all the lookups?) and we need to use _that_
        // namer in lookup_in_trees, not this one.
        unimplemented!()
    }

    fn lookup_in_root(&mut self, name: &Name, crumbs: &HashTrieSet<Scope>) -> Vec<Located<Decl>> {
        if crumbs.contains(&Scope::Global) {
            return vec![];
        }

        // we only search other bundles for legal bundle names
        if ! name.is_bundle_name() {
            return vec![];
        }

        let results = self.lookup_in_trees(&ScopeGraph::get_loaded_trees(), name, &crumbs.insert(Scope::Global));

        if ScopeGraph::all_mixfix(&results) {
            if let Some(t) = ScopeGraph::load_from_bundle(name) {
                return self.lookup_in_trees(&vec![t], name, &crumbs.insert(Scope::Global))
            }
        }

        results
    }

    fn lookup_in_trees(&mut self, trees: &Vec<trees::Root>, name: &Name, crumbs: &HashTrieSet<Scope>) -> Vec<Located<Decl>> {
        let mut results = Vec::new();

        for tree in trees {
            match tree {
                trees::Root::Bundle { scope_id, .. } => {
                    let scope = match scope_id {
                        trees::ScopeId::Empty => Scope::Empty,
                        trees::ScopeId::Global => Scope::Global,
                        trees::ScopeId::Scope(x) => Scope::Env(EnvIndex(*x)),
                    };
                    let mut v = self.lookup_here_in_scope(scope, name, crumbs);
                    results.append(&mut v);
                },
            }
        }

        results
    }

    fn lookup_in_imports(&mut self, imports: &Vec<Located<Import>>, x: &Name, crumbs: &HashTrieSet<Scope>) -> Vec<Located<Decl>> {
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
            let r = self.lookup_here_in_scope(scope, &x, crumbs);
            results.append(&mut r.clone());
        }

        results
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

    fn lookup_here_in_scope(&mut self, scope: Scope, name: &Name, crumbs: &HashTrieSet<Scope>) -> Vec<Located<Decl>> {
        if crumbs.contains(&scope) {
            return vec![];
        }

        match scope {
            Scope::Empty => vec![],
            Scope::Global => self.lookup_in_root(name, crumbs),
            Scope::Lookup(LookupIndex(index)) => {
                let r = self.lookups.get(index).unwrap();
                let decls = self.do_lookup(r.clone(), &crumbs.insert(scope));
                let frames = decls.iter().flat_map(|d| {
                    match d {
                        Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                        _ => vec![]
                    }
                });
                let inner_decls = frames.flat_map(|s| {
                    self.lookup_here_in_scope(s, name, &crumbs.insert(scope))
                }).collect();
                inner_decls
            },
            Scope::LookupHere(LookupHereIndex(index)) => {
                let r = self.lookups_here.get(index).unwrap();
                let decls = self.do_lookup_here(r.clone(), &crumbs.insert(scope));
                let frames = decls.iter().flat_map(|d| {
                    match d {
                        Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                        _ => vec![]
                    }
                });
                let inner_decls = frames.flat_map(|s| {
                    self.lookup_here_in_scope(s, name, &crumbs.insert(scope))
                }).collect();
                inner_decls
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let r = self.mixfixes.get(index).unwrap();
                let trees = self.do_mixfix(r.clone(), &crumbs.insert(scope));
                let decls = trees.iter().flat_map(|Located { ref loc, value: ref t }| ScopeGraph::mixfix_tree_to_decls(t));
                let frames = decls.flat_map(|d| {
                    match d {
                        Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                        _ => vec![]
                    }
                });
                let inner_decls = frames.flat_map(|s| {
                    self.lookup_here_in_scope(s, name, &crumbs.insert(scope))
                }).collect();
                inner_decls
            },
            Scope::Env(EnvIndex(index)) => {
                let env = self.envs.get(index).unwrap();
                // need to clone the imports and parents
                // before we borrow self mutably.
                let imports = env.imports.clone();
                let parents = env.parents.clone();
                let includes = env.includes.clone();
                let found_here = self.lookup_here_in_scope(scope, name, crumbs);

                let mut results = Vec::new();
                results.append(&mut found_here.clone());

                for include in includes {
                    let v = self.lookup_here_in_scope(include, name, &crumbs.insert(scope));
                    let mut w = v.clone();
                    results.append(&mut w);
                }

                results
            }
        }
    }

    fn lookup_in_scope(&mut self, scope: Scope, name: &Name, crumbs: &HashTrieSet<Scope>) -> Vec<Located<Decl>> {
        if crumbs.contains(&scope) {
            return vec![];
        }

        match scope {
            Scope::Empty => vec![],
            Scope::Global => self.lookup_in_root(name, crumbs),
            Scope::Lookup(LookupIndex(index)) => {
                let r = self.lookups.get(index).unwrap();
                let decls = self.do_lookup(r.clone(), &crumbs.insert(scope));
                let frames = decls.iter().flat_map(|d| {
                    match d {
                        Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                        _ => vec![]
                    }
                });
                let inner_decls = frames.flat_map(|s| {
                    self.lookup_in_scope(s, name, &crumbs.insert(scope))
                }).collect();
                inner_decls
            },
            Scope::LookupHere(LookupHereIndex(index)) => {
                let r = self.lookups_here.get(index).unwrap();
                let decls = self.do_lookup_here(r.clone(), &crumbs.insert(scope));
                let frames = decls.iter().flat_map(|d| {
                    match d {
                        Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                        _ => vec![]
                    }
                });
                let inner_decls = frames.flat_map(|s| {
                    self.lookup_in_scope(s, name, &crumbs.insert(scope))
                }).collect();
                inner_decls
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let r = self.mixfixes.get(index).unwrap();
                let trees = self.do_mixfix(r.clone(), &crumbs.insert(scope));
                let decls = trees.iter().flat_map(|Located { ref loc, value: ref t }| ScopeGraph::mixfix_tree_to_decls(t));
                let frames = decls.flat_map(|d| {
                    match d {
                        Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                        _ => vec![]
                    }
                });
                let inner_decls = frames.flat_map(|s| {
                    self.lookup_in_scope(s, name, &crumbs.insert(scope))
                }).collect();
                inner_decls
            },
            Scope::Env(EnvIndex(index)) => {
                let env = self.envs.get(index).unwrap();
                // need to clone the imports and parents
                // before we borrow self mutably.
                let imports = env.imports.clone();
                let parents = env.parents.clone();
                let includes = env.includes.clone();
                let found_here = self.lookup_here_in_scope(scope, name, crumbs);

                let mut results = Vec::new();
                results.append(&mut found_here.clone());

                if ScopeGraph::all_mixfix(&found_here) {
                    let rs = self.lookup_in_imports(&imports, name, &crumbs.insert(scope));
                    let found_imports = ScopeGraph::filter_mixfix(rs, &results);

                    results.append(&mut found_imports.clone());

                    if ScopeGraph::all_mixfix(&found_imports) {
                        let mut ps = Vec::new();
                        for parent in parents {
                            let v = self.lookup_in_scope(parent, name, &crumbs.insert(scope));
                            let mut w = v.clone();
                            ps.append(&mut w);
                        }

                        let found_parents = ScopeGraph::filter_mixfix(ps, &results);
                        results.append(&mut found_parents.clone());
                    }
                }

                for include in includes {
                    let v = self.lookup_in_scope(include, name, &crumbs.insert(scope));
                    let mut w = v.clone();
                    results.append(&mut w);
                }

                results
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
