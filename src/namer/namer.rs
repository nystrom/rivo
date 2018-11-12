// ScopeGraph maintains the scope graph.

use syntax::loc::*;
use syntax::names::*;
use syntax::trees;

use super::symbols::*;
use super::graph::*;
use super::earley::*;

use driver;
use driver::*;
use driver::bundle::*;

use std::collections::HashMap;
use std::collections::HashSet;
use rpds::HashTrieSet;

use std::cmp::min;

#[cfg(debug_assertions)]
#[allow(non_upper_case_globals)]
static mut depth: u32 = 0;

type Crumb = LookupRef;
type Crumbs = HashTrieSet<Crumb>;

// internal cache
#[derive(Debug)]
pub struct Cache<'a> {
    pub lookup_cache: &'a mut HashMap<LookupRef, Vec<Located<Decl>>>,
    pub mixfix_cache: &'a mut HashMap<MixfixRef, Vec<MixfixTree>>,
}

// This is a public lookup result.
type LookupResult<T> = Result<T, Located<String>>;

// TODO: make driver a trait for loading files.
pub struct Namer<'a> {
    pub graph: &'a ScopeGraph,
    pub driver: &'a mut driver::Driver,
    pub cache: &'a mut Cache<'a>,
}

// As we perform a lookup, we go through several scopes, caching the intermediate results.
// We can cache a result if it does not depend on anything further down the call stack.
// We pass the stack around in a HashTrieSet.
// If we hit a cycle, we return the offending scope so we can check if we should cache the result.

impl<'a> Namer<'a> {
    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    pub fn lookup(&mut self, r: &LookupRef) -> LookupResult<Vec<Located<Decl>>> {
        match r {
            LookupRef { scope, name } => {
                let mut decls = Vec::new();
                let crumbs = HashTrieSet::new();
                self.lookup_in_scope(*scope, &name, &crumbs, &mut decls)?;
                Ok(decls)
            }
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    pub fn parse_mixfix(&mut self, r: &MixfixRef) -> LookupResult<Vec<MixfixTree>> {
        match r {
            MixfixRef { parts } => {
                let mut trees = Vec::new();
                let crumbs = HashTrieSet::new();
                self.resolve_mixfix(&parts, &crumbs, &mut trees)?;
                Ok(trees)
            }
        }
    }

    // We return the crumbs that hit a cycle _intersected_ with the incoming crumbs.
    // So if the empty set is returned, caching is allowed.

    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    fn resolve_mixfix(&mut self, parts: &Vec<MixfixPart>, crumbs: &Crumbs, results: &mut Vec<MixfixTree>) -> LookupResult<Crumbs> {
        match self.cache.mixfix_cache.get(&MixfixRef { parts: parts.clone() }) {
            Some(trees) => {
                println!("mixfix cache hit");
                self.driver.stats.accum("mixfix cache hit", 1);
                results.extend(trees.iter().cloned());
                return Ok(HashTrieSet::new());
            },
            None => {
                println!("mixfix cache miss");
            },
        }

        self.driver.stats.accum("mixfix expression size", parts.len() as u64);

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
        let mut cycles = HashTrieSet::new();

        for r in refs {
            match r {
                Some(r) => {
                    let mut decls = Vec::new();

                    match &r {
                        LookupRef { scope, name } => {
                            let vs = self.lookup_in_scope(*scope, &name, &crumbs, &mut decls)?;
                            for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                        }
                    }

                    if ! decls.is_empty() && Namer::all_mixfix(&decls) {
                        match &r.name {
                            Name::Id(x) => {
                                tokens.push(Token::Name(Part::Id(x.to_owned()), decls.clone()));
                                has_name = true;
                            },
                            Name::Op(x) => {
                                tokens.push(Token::Name(Part::Op(x.to_owned()), decls.clone()));
                                has_name = true;
                            },
                            _ => {
                                // The name is a mixfix name, so can't be a mixfix part.
                                // This should be an error, since we shouldn't lookup a mixfix name
                                // and get a mixfix part, but just roll with it. We'll get an error
                                // eventually.
                                tokens.push(Token::Exp(MixfixTree::Exp));
                            },
                        }
                    }
                    else {
                        // Either the name resolved at least one non-mixfix declaration
                        // or it resolved to nothing. In the latter case, we'll get an error later during
                        // renaming, but treat it like an expression so mixfix parsing might
                        // possibly succeed and we won't report an error for that too.
                        // This can happen, for instance, with `x + y` where `x` or `y` is not defined.
                        // We want an error for those, but not for `+` too.
                        tokens.push(Token::Exp(MixfixTree::Exp));
                    }
                },
                None => {
                    // The source expression is a non-name expression.
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

            results.push(tree);

            // We didn't hit any cycles
            if cycles.is_empty() {
                self.cache.mixfix_cache.insert(MixfixRef { parts: parts.clone() }, results.clone());
            }

            Ok(cycles)
        }
        else {
            // Resolve the mixfix expression.

            // We don't include decls whose mixfix name includes parts that are not in the tokens list.
            // This will reduce the number of decls to consider during mixfix parsing and therefore reduce the size of the grammar and therefore simplify the mixfix parser.
            let mut parts_in_tokens = HashSet::new();

            for t in &tokens {
                match t {
                    Token::Name(x, _) => { parts_in_tokens.insert(x); },
                    _ => {},
                }
            }

            let mut decls: Vec<Located<Decl>> = Vec::new();

            for t in &tokens {
                match t {
                    Token::Name(_, xdecls) => {
                        for xd in xdecls {
                            let loc = xd.loc;
                            let decl = match &xd.value {
                                Decl::MixfixPart { orig, .. } => Located::new(loc, *orig.clone()),
                                decl => Located::new(loc, decl.clone()),
                            };
                            match decl.name() {
                                Name::Mixfix(s) => {
                                    let parts = Name::decode_parts(s);
                                    if parts.iter().all(|part| part == &Part::Placeholder || parts_in_tokens.contains(part)) {
                                        decls.push(decl);
                                    }
                                },
                                _ => {},
                            }
                        }
                    },
                    _ => {},
                }
            }

            println!("parts {:?}", parts);
            println!("tokens {:?}", tokens);
            println!("decls {:?}", decls);

            self.driver.stats.accum("mixfix decls size", decls.len() as u64);

            // This should not happen, but just return anyway.
            if decls.is_empty() {
                return Ok(cycles);
            }

            let mut trees = {
                // if all decls have the same name, we can use fast parsing.
                let mut names: Vec<Name> = decls.iter().map(|d| d.name()).collect();
                names.sort();
                names.dedup();

                if names.len() == 1 {
                    self.driver.stats.accum("mixfix fast parse", 1);

                    let timer = self.driver.stats.start_timer();

                    let trees = match Namer::fast_parse(tokens, &decls, names.first().unwrap()) {
                        Some(t) => vec![t],
                        None => vec![],
                    };

                    self.driver.stats.end_timer("fast mixfix parsing time", timer);

                    trees
                }
                else {
                    self.driver.stats.accum("mixfix Earley parse decls size", decls.len() as u64);

                    let timer = self.driver.stats.start_timer();

                    let trees = Earley::new(&decls).parse(&tokens);

                    self.driver.stats.end_timer("earley parsing time", timer);

                    trees
                }
            };

            results.append(&mut trees);

            // We didn't hit any cycles
            if cycles.is_empty() {
                self.cache.mixfix_cache.insert(MixfixRef { parts: parts.clone() }, results.clone());
            }

            Ok(cycles)
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    fn lookup_in_root(&mut self, name: &Name, crumbs: &Crumbs, results: &mut Vec<Located<Decl>>) -> LookupResult<Crumbs> {
        self.driver.stats.accum("lookup_in_root", 1);

        // we only search other bundles for legal bundle names
        if ! name.is_bundle_name() {
            self.driver.stats.accum("lookup_in_root not bundle name", 1);
            return Ok(HashTrieSet::new());
        }

        // Check if we hit a cycle.
        let crumb = LookupRef { scope: Scope::Global, name: name.clone() };

        if crumbs.contains(&crumb) {
            return Ok(HashTrieSet::new().insert(crumb));
        }

        let new_crumbs = crumbs.insert(crumb.clone());

        let mut cycles = self.lookup_in_trees(name, &new_crumbs, results)?;

        // If all tree results were mixfix parts, try to load a bundle with the same name.
        // If successful, add those results to the results.
        if Namer::all_mixfix(&results) {
            match self.load_bundle_by_name(name) {
                Ok(index) => {
                    match self.driver.name_bundle(index) {
                        Ok(_) => {},
                        Err(msg) => {
                            return Err(msg)
                        },
                    }

                    match self.driver.get_bundle(index) {
                        Some(Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let vs = self.lookup_in_scope(scope.to_here(), name, &new_crumbs, results)?;
                                for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                            }
                            else {
                                self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                            }
                        },
                        Some(Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(scope) = scopes.get(&id) {
                                let vs = self.lookup_in_scope(scope.to_here(), name, &new_crumbs, results)?;
                                for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                            }
                            else {
                                self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                            }
                        },
                        Some(Bundle::Core { root_scope, .. }) => {
                            let vs = self.lookup_in_scope(root_scope.to_here(), name, &new_crumbs, results)?;
                            for v in vs.iter() { cycles = cycles.insert(v.clone()); }
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

        // Return all the cycles we found, except the ones that hit exactly here.
        Ok(cycles.remove(&crumb))
    }

    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    fn load_bundle_by_name(&mut self, name: &Name) -> LookupResult<BundleIndex> {
        self.driver.load_bundle_by_name(name)
    }

    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    fn lookup_in_trees(&mut self, name: &Name, crumbs: &Crumbs, results: &mut Vec<Located<Decl>>) -> LookupResult<Crumbs> {
        self.driver.stats.accum("lookup_in_trees", 1);

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

        let crumb = (Scope::Global, name.clone());

        // HACK
        // We want to call lookup_in_scope with EnvWithoutImports(env), but can't since we have to
        // mutably borrow driver while we're looping over the bundles and we don't want to clone
        // the bundles (which are huge).
        let stats = &mut self.driver.stats;

        for bundle in &self.driver.bundles {
            match bundle {
                Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
                    if let Some(Scope::Env(env)) = scopes.get(&id) {
                        Namer::lookup_here_in_env(stats, graph, *env, name, crumbs, results)?;
                    }
                },
                Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, graph, .. } => {
                    if let Some(Scope::Env(env)) = scopes.get(&id) {
                        Namer::lookup_here_in_env(stats, graph, *env, name, crumbs, results)?;
                    }
                },
                // Bundle::Core { root_scope, graph, .. } => {
                //     to_search.push(*root_scope);
                // },
                _ => {},
            }
        }

        self.driver.stats.accum("lookup_in_trees size", results.len() as u64);

        Ok(HashTrieSet::new())
    }

    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    fn lookup_in_imports(&mut self, imports: &Vec<Located<Import>>, x: &Name, crumbs: &Crumbs, results: &mut Vec<Located<Decl>>) -> LookupResult<Crumbs> {
        let paths = Namer::import_paths(imports, x);

        // FIXME: clarify this! We want to lookup p.x. When resolving _p_, we should not search the other imports.

        let mut cycles = HashTrieSet::new();

        for (scope, x) in &paths {
            // When looking up an imported name, we should search the parents of the scope, but not the other imports, otherwise we'll find ourselves.
            // let mut new_crumbs = HashTrieSet::new();
            //
            // for (scope, x) in &paths {
            //     match scope {
            //         Scope::Env(env_index) => { new_crumbs.insert(env_index.clone()); },
            //         Scope::EnvWithoutImports(env_index) => { new_crumbs.insert(env_index.clone()); },
            //         _ => {},
            //     }
            // }
            // match scope {
            //     Scope::Env(env_index) => { new_crumbs.remove(&env_index); },
            //     Scope::EnvWithoutImports(env_index) => { new_crumbs.remove(&env_index); },
            //     _ => {},
            // }
            //
            // // IMPORTANT: add the old crumbs AFTER the remove in case the (scope, x) is in crumbs already.
            // for crumb in crumbs {
            //     new_crumbs = new_crumbs.insert(crumb.clone());
            // }

            let vs = self.lookup_in_scope(scope.to_here(), x, crumbs, results)?;
            for v in vs.iter() { cycles = cycles.insert(v.clone()); }
        }

        self.driver.stats.accum("lookup_in_imports size", results.len() as u64);

        Ok(cycles)
    }

    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    fn get_inner_decls(&mut self, name: &Name, decls: &Vec<Located<Decl>>, crumbs: &Crumbs, results: &mut Vec<Located<Decl>>) -> LookupResult<Crumbs> {
        let frames = decls.iter().flat_map(|d| {
            match d {
                Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                _ => vec![]
            }
        });

        let mut cycles = HashTrieSet::new();

        for s in frames {
            let vs = self.lookup_in_scope(s.to_here(), name, crumbs, results)?;
            for v in vs.iter() { cycles = cycles.insert(v.clone()); }
        }

        Ok(cycles)
    }

    // HACK
    // Version of lookup_here_in_scope that only works on Env environments.
    // Called from lookup_in_trees. Avoids loading other bundles which requires
    // borrowing the driver mutably. The assumption is that the root scope of a bundle
    // is always a simple Env.
    #[cfg_attr(debug_assertions, trace(disable(crumbs, stats, graph)))]
    fn lookup_here_in_env(stats: &mut driver::stats::Stats, graph: &ScopeGraph, env_index: EnvIndex, name: &Name, crumbs: &Crumbs, results: &mut Vec<Located<Decl>>) -> LookupResult<Crumbs> {
        let crumb = LookupRef { scope: Scope::Env(env_index), name: name.clone() };

        if crumbs.contains(&crumb) {
            stats.accum("lookup_here_in_env cycle", 1);
            return Ok(HashTrieSet::new().insert(crumb));
        }

        let new_crumbs = crumbs.insert(crumb.clone());
        let mut cycles = HashTrieSet::new();

        match env_index {
            EnvIndex(index) => {
                let env = graph.envs.get(index).unwrap();
                // need to clone the includes
                // before we borrow self mutably.
                let includes = env.includes.clone();

                let tmp = vec![];
                let found_here = env.decls.get(name).unwrap_or(&tmp);

                results.extend(found_here.iter().cloned());

                for include in includes {
                    match include {
                        Scope::Env(include) if include != env_index => {
                            let vs = Namer::lookup_here_in_env(stats, graph, include, name, &new_crumbs, results)?;
                            for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                        },
                        Scope::EnvWithoutImports(include) if include != env_index => {
                            let vs = Namer::lookup_here_in_env(stats, graph, include, name, &new_crumbs, results)?;
                            for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                        },
                        _ => {},
                    }
                }
            }
        }

        Ok(cycles.remove(&crumb))
    }

    #[cfg_attr(debug_assertions, trace(disable(crumbs)))]
    fn lookup_in_scope(&mut self, scope: Scope, name: &Name, crumbs: &Crumbs, results: &mut Vec<Located<Decl>>) -> LookupResult<Crumbs> {
        let crumb = LookupRef { scope, name: *name };

        match self.cache.lookup_cache.get(&crumb) {
            Some(decls) => {
                println!("lookup_cache hit");
                self.driver.stats.accum("lookup_cache hit size", decls.len() as u64);
                results.extend(decls.iter().cloned());
                return Ok(HashTrieSet::new());
            },
            None => {
                println!("lookup_cache miss");
                self.driver.stats.accum("lookup_cache miss", 1);
            },
        }

        // Check if we hit a cycle.
        if crumbs.contains(&crumb) {
            self.driver.stats.accum("lookup_in_scope cycle", 1);
            return Ok(HashTrieSet::new().insert(crumb));
        }

        // FIXME: Even if we didn't hit a cycle, if we've seen this crumb before we can just return.
        // But we shouldn't necessarily cache the results.

        let new_crumbs = crumbs.insert(crumb.clone());

        let result = match scope {
            Scope::Empty => Ok(HashTrieSet::new()),
            Scope::Global => self.lookup_in_root(name, &new_crumbs, results),
            Scope::Lookup(LookupIndex(index)) => {
                let r = self.graph.lookups.get(index).unwrap();

                match &r {
                    LookupRef { scope: r_scope, name: r_name } => {
                        let mut decls = Vec::new();
                        let mut cycles = HashTrieSet::new();

                        let vs = self.lookup_in_scope(*r_scope, &r_name, &new_crumbs, &mut decls)?;
                        for v in vs.iter() { cycles = cycles.insert(v.clone()); }

                        let vs = self.get_inner_decls(&name, &decls, &new_crumbs, results)?;
                        for v in vs.iter() { cycles = cycles.insert(v.clone()); }

                        Ok(cycles)
                    }
                }
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let r = self.graph.mixfixes.get(index).unwrap();

                match &r {
                    MixfixRef { parts } => {
                        let mut trees = Vec::new();
                        let mut cycles = HashTrieSet::new();

                        let vs = self.resolve_mixfix(parts, &new_crumbs, &mut trees)?;
                        for v in vs.iter() { cycles = cycles.insert(v.clone()); }

                        let decls = trees.iter().flat_map(|t| Namer::mixfix_tree_to_decls(t)).collect();

                        let vs = self.get_inner_decls(&name, &decls, &new_crumbs, results)?;
                        for v in vs.iter() { cycles = cycles.insert(v.clone()); }

                        Ok(cycles)
                    }
                }
            },
            Scope::EnvWithoutImports(EnvIndex(index)) => {
                let env = self.graph.envs.get(index).unwrap();
                // need to clone the imports and parents and includes
                // before we borrow self mutably.
                let imports = env.imports.clone();
                let parents = env.parents.clone();
                let includes = env.includes.clone();

                let mut cycles = HashTrieSet::new();

                let tmp = vec![];
                let found_here = env.decls.get(name).unwrap_or(&tmp);

                results.extend(found_here.iter().cloned());

                if Namer::all_mixfix(&results) {
                    let mut ps = Vec::new();

                    for parent in parents {
                        if self.imports_truncated(&imports, parent) {
                            continue;
                        }
                        let vs = self.lookup_in_scope(parent, name, &new_crumbs, &mut ps)?;
                        for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                    }

                    // FIXME: push the filter into the recursive call.
                    // We want to NOT add any mixfix decls whose full name is already in the results.
                    let found_parents = Namer::filter_mixfix(ps, &found_here);
                    results.extend(found_parents.iter().cloned());
                }

                // TODO: speed things up by adding the other includes to the cache as vec![].
                // Don't add to the crumbs because we don't want to break cycle detection.
                // We want to just truncate the search when we hit the include because we'll
                // hit them again... then again, if we hit an include, the result should get cached
                // there and we'll just return immediately.

                for include in includes {
                    let vs = self.lookup_in_scope(include, name, &new_crumbs, results)?;
                    for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                }

                Ok(cycles.remove(&crumb))
            }
            Scope::EnvHere(EnvIndex(index)) => {
                let env = self.graph.envs.get(index).unwrap();
                // need to clone the imports and parents and includes
                // before we borrow self mutably.
                let includes = env.includes.clone();

                let mut cycles = HashTrieSet::new();

                let tmp = vec![];
                let found_here = env.decls.get(name).unwrap_or(&tmp);

                results.extend(found_here.iter().cloned());

                for include in includes {
                    let vs = self.lookup_in_scope(include.to_here(), name, &new_crumbs, results)?;
                    for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                }

                Ok(cycles.remove(&crumb))
            }
            Scope::Env(EnvIndex(index)) => {
                let env = self.graph.envs.get(index).unwrap();
                // need to clone the imports and parents and includes
                // before we borrow self mutably.
                let imports = env.imports.clone();
                let parents = env.parents.clone();
                let includes = env.includes.clone();

                let mut cycles = HashTrieSet::new();

                let tmp = vec![];
                let found_here = env.decls.get(name).unwrap_or(&tmp);

                let mut seen = Vec::new();
                seen.extend(found_here.iter().cloned());

                if Namer::all_mixfix(&results) {
                    // when searching imports, put both the env and env-without-imports in the crumbs.
                    let mut rs = Vec::new();

                    let vs = self.lookup_in_imports(&imports, name, &new_crumbs, &mut rs)?;
                    for v in vs.iter() { cycles = cycles.insert(v.clone()); }

                    let found_imports = Namer::filter_mixfix(rs, &seen);
                    seen.extend(found_imports.iter().cloned());
                }

                if Namer::all_mixfix(&results) {
                    let mut ps = Vec::new();

                    for parent in parents {
                        if self.imports_truncated(&imports, parent) {
                            continue;
                        }
                        let vs = self.lookup_in_scope(parent, name, &new_crumbs, &mut ps)?;
                        for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                    }

                    let found_parents = Namer::filter_mixfix(ps, &seen);
                    seen.extend(found_parents.iter().cloned());
                }

                results.extend(seen.iter().cloned());

                for include in includes {
                    let vs = self.lookup_in_scope(include, name, &new_crumbs, results)?;
                    for v in vs.iter() { cycles = cycles.insert(v.clone()); }
                }

                Ok(cycles.remove(&crumb))
            }
        };

        match &result {
            Ok(cycles) => {
                if cycles.is_empty() {
                    self.cache.lookup_cache.insert(crumb, results.clone());
                }
                self.driver.stats.accum("lookup result size", results.len() as u64);
            },
            _ => {
                self.driver.stats.accum("lookup error", 1);
            },
        }

        result
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Utility functions
    ////////////////////////////////////////////////////////////////////////////////

    // Do fast mixfix parsing. Should be called only when all the decls have a single mixfix name.
    // It just checks that the tokens match the mixfix name and builds the call tree.
    fn fast_parse(tokens: Vec<Token>, decls: &Vec<Located<Decl>>, name: &Name) -> Option<MixfixTree> {
        match name {
            Name::Mixfix(s) => {
                let parts = Name::decode_parts(*s);

                // Check that the tokens agree with the mixfix name.
                if parts.len() != tokens.len() {
                    return None;
                }

                // Placeholders should line up with expression tokens.
                // Names should line up with name tokens.
                for (part, token) in parts.iter().zip(tokens.iter()) {
                    match (part, token) {
                        (Part::Placeholder, Token::Exp(_)) => {},
                        (Part::Id(x), Token::Name(Part::Id(y), _)) => {
                            if x != y {
                                return None;
                            }
                        },
                        (Part::Op(x), Token::Name(Part::Op(y), _)) => {
                            if x != y {
                                return None;
                            }
                        },
                        _ => {
                            return None;
                        },
                    }
                }

                // All ok, now make the expression tree.
                let args: Vec<MixfixTree> = tokens.iter().filter_map(|t| match t {
                    Token::Exp(e) => Some(e.clone()),
                    _ => None,
                }).collect();

                Some(MixfixTree::make_call(MixfixTree::Name(name.clone(), decls.clone()), &args))
            },
            _ => {
                None
            },
        }
    }

    fn import_paths(imports: &Vec<Located<Import>>, x: &Name) -> Vec<(Scope, Name)> {
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

        // Sort the paths and remove duplicates (sorting is needed just because dedup only removes consequetive dups).
        paths.sort();
        paths.dedup();

        paths
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
