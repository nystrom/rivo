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

use std::collections::VecDeque;
use std::collections::HashMap;
use std::collections::HashSet;
use rpds::HashTrieSet;

use std::cmp::min;

#[cfg(debug_assertions)]
#[allow(non_upper_case_globals)]
static mut depth: usize = 0;

#[cfg(debug_assertions)]
macro_rules! trace {
    ($($arg:tt)*) => { print!("{1:0$}[!] At {2}:{3}: ", depth, " ", file!(), line!()); println!($($arg)*) };
}

#[cfg(not(debug_assertions))]
macro_rules! trace {
    ($($arg:tt)*) => { };
}

type Trees = Vec<MixfixTree>;
type Decls = Vec<Located<Decl>>;
type LookupCache = HashMap<LookupRef, Decls>;
type TreeCache = HashMap<MixfixRef, Trees>;

type NamerResult<T> = Result<T, Located<String>>;

// internal cache
#[derive(Debug)]
pub struct Cache<'a> {
    pub lookup_cache: &'a mut LookupCache,
    pub tree_cache: &'a mut TreeCache,
}

// TODO: make driver a trait for loading files.
pub struct Namer<'a> {
    pub driver: &'a mut driver::Driver,
    pub cache: &'a mut Cache<'a>,
}

type Visited = LookupCache;
type Dependencies = HashTrieSet<LookupRef>;

// #[derive(Debug)]
struct Worklist {
    changed: bool,
    worklist: VecDeque<LookupRef>,
    in_worklist: HashSet<LookupRef>,
}

use std::fmt;

impl fmt::Debug for Worklist {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.worklist.fmt(f)
    }
}

impl Worklist {
    pub fn new() -> Self {
        Worklist {
            changed: true,
            worklist: VecDeque::new(),
            in_worklist: HashSet::new()
        }
    }

    pub fn push_back(&mut self, r: LookupRef) {
        if self.in_worklist.contains(&r) {
            return;
        }
        self.changed = true;
        self.in_worklist.insert(r);
        self.worklist.push_back(r)
    }

    pub fn pop_front(&mut self) -> Option<LookupRef> {
        if let Some(r) = self.worklist.pop_front() {
            self.in_worklist.remove(&r);
            Some(r)
        }
        else {
            None
        }
    }

    pub fn is_changed(&self) -> bool {
        self.changed
    }

    pub fn reset_changed(&mut self) {
        self.changed = false;
    }

    pub fn is_empty(&self) -> bool {
        self.worklist.is_empty()
    }

    pub fn contains(&self, r: &LookupRef) -> bool {
        self.in_worklist.contains(r)
    }
}

pub trait Lattice<Rhs = Self> : PartialOrd<Rhs> where Rhs: ?Sized {
    fn top() -> Rhs;
    fn bot() -> Rhs;
    fn lub(&self, other: &Rhs) -> Rhs;
    fn glb(&self, other: &Rhs) -> Rhs;
    fn height(&self) -> usize;
}

use std::cmp::Ordering;
use std::hash::Hash;

#[derive(Debug, PartialEq, Clone)]
pub struct Set<A: Eq + Hash + Clone>(HashTrieSet<A>);

impl<A> Lattice for Set<A> where A: Eq + Hash + Clone {
    fn bot() -> Set<A> { Set(HashTrieSet::new()) }
    fn top() -> Set<A> { unimplemented!() }
    fn lub(&self, other: &Set<A>) -> Set<A> {
        let mut s = HashTrieSet::new();
        for x in self.0.iter() { s = s.insert(x.clone()); }
        for x in other.0.iter() { s = s.insert(x.clone()); }
        Set(s)
    }
    fn glb(&self, other: &Set<A>) -> Set<A> {
        let mut s = HashTrieSet::new();
        for x in self.0.iter() { if other.0.contains(&x) { s = s.insert(x.clone()); } }
        Set(s)
    }
    fn height(&self) -> usize {
        self.0.size()
    }
}

impl<A> PartialOrd for Set<A> where A: Eq + Hash + Clone {
    fn partial_cmp(&self, other: &Set<A>) -> Option<Ordering> {
        let leq = self.0.is_subset(&other.0);
        let geq = self.0.is_superset(&other.0);
        if leq && geq {
            Some(Ordering::Equal)
        }
        else if leq {
            Some(Ordering::Less)
        }
        else if geq {
            Some(Ordering::Greater)
        }
        else {
            None
        }
    }
}


impl<'a> Namer<'a> {
    // The general naming strategy is rather complicated.
    // To lookup a name in an environment, we look:
    // - in the current environment
    // - in any the imported environments
    // - in any parent environments
    // - in any included environments
    // The main complications are imports and includes, which can be mutually recursive.
    // Thus, we keep looking until we hit a fixpoint in the lookup.

    // Each time we traverse an "edge" of the scope graph, we check the previous value computed
    // for that edge. If the new value is different from the previous value, we add the edge
    // to a worklist to recompute it. When the worklist is empty, we stop.

    // If we hit an cycle in the graph, we add the node to the worklist.

    #[cfg_attr(debug_assertions, trace)]
    pub fn lookup(&mut self, r: &LookupRef) -> NamerResult<Decls> {
        if let Some(results) = self.cache.lookup_cache.get(r) {
            trace!("lookup {:?} cached", r);
            self.driver.stats.accum("root lookup_cache hit", 1);
            return Ok(results.clone());
        }

        // Stats
        let timer = self.driver.stats.start_timer();
        let mut iters = 0;

        let mut worklist = Worklist::new();
        worklist.push_back(r.clone());

        let mut results = Vec::new();
        let mut visited = HashMap::new();

        while let Some(r) = worklist.pop_front() {
            trace!("lookup loop {:?}", r);
            iters += 1;
            worklist.reset_changed();
            results = self.try_lookup_ref(r, &mut worklist, &mut visited, &HashTrieSet::new())?;
        }

        // Update the cache with the visited nodes.
        // This might not be needed...
        self.cache.lookup_cache.extend(visited.drain());

        if iters > 10 {
            println!("iters for {:?} = {}", r, iters);
            println!("time for {:?} = {} ms", r, timer.elapsed().as_millis());
        }

        self.driver.stats.end_timer("lookup time", timer);
        self.driver.stats.accum("lookup loop iters", iters);

        Ok(results)
    }

    #[cfg_attr(debug_assertions, trace)]
    pub fn parse_mixfix(&mut self, m: &MixfixRef) -> NamerResult<Trees> {
        if let Some(results) = self.cache.tree_cache.get(m) {
            trace!("parse_mixfix {:?} cached", m);
            return Ok(results.clone());
        }

        let timer = self.driver.stats.start_timer();
        let mut inner_iters = 0;
        let mut outer_iters = 0;

        let mut visited = HashMap::new();

        loop {
            let mut worklist = Worklist::new();

            outer_iters += 1;

            let results = self.try_mixfix_ref(m.clone(), &mut worklist, &mut visited, &HashTrieSet::new())?;

            if worklist.is_empty() {
                // Update the cache with the visited nodes.
                // This might not be needed...
                self.cache.lookup_cache.extend(visited.drain());

                // Update the tree cache.
                self.cache.tree_cache.insert(m.clone(), results.clone());

                self.driver.stats.end_timer("parse_mixfix time", timer);
                self.driver.stats.accum("parse_mixfix outer iters", outer_iters);
                self.driver.stats.accum("parse_mixfix inner iters", inner_iters);

                return Ok(results)
            }

            while let Some(r) = worklist.pop_front() {
                worklist.reset_changed();
                inner_iters += 1;
                self.try_lookup_ref(r, &mut worklist, &mut visited, &HashTrieSet::new())?;
            }
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(worklist, visited, stack)))]
    fn try_mixfix_ref(&mut self, r: MixfixRef, worklist: &mut Worklist, visited: &mut Visited, stack: &Dependencies) -> NamerResult<Trees> {
        if let Some(result) = self.cache.tree_cache.get(&r) {
            // Already completed.
            return Ok(result.clone());
        }

        self.driver.stats.accum("mixfix expression size", r.parts.len() as u64);

        let mut refs: Vec<Option<LookupRef>> = vec![];

        for part in &r.parts {
            match part.name_ref {
                Some(LookupIndex(index)) => {
                    let r = self.driver.graph.lookups.get(index);
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
                    let decls = self.try_lookup_ref(r, worklist, visited, stack)?;

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

        let mut results = Vec::new();

        if ! has_name {
            // If there are no names in the mixfix expression, we left-associate the parts into a call.
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

            let mut decls: Decls = Vec::new();

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

            trace!("parts {:?}", r.parts);
            trace!("tokens {:?}", tokens);
            trace!("decls {:?}", decls);

            self.driver.stats.accum("mixfix decls size", decls.len() as u64);

            // The names in the expression did not resolve into anything we can possibly parse.
            // Return early.
            if decls.is_empty() {
                return Ok(vec![]);
            }

            let mut trees = {
                // if all decls have the same name, we can use fast parsing.
                let mut names: Vec<Name> = decls.iter().map(|d| d.name()).collect();
                names.sort();
                names.dedup();

                if names.len() == 1 {
                    self.driver.stats.accum("mixfix fast parse", 1);

                    let timer = self.driver.stats.start_timer();

                    let trees = match Namer::fast_parse(tokens, &decls, *names.first().unwrap()) {
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
        }

        Ok(results)
    }

    #[cfg_attr(debug_assertions, trace(disable(worklist, visited, stack)))]
    fn lookup_in_root(&mut self, name: Name, worklist: &mut Worklist, visited: &mut Visited, stack: &Dependencies) -> NamerResult<Decls> {
        self.driver.stats.accum("lookup_in_root", 1);

        // we only search other bundles for legal bundle names
        if ! name.is_bundle_name() {
            self.driver.stats.accum("lookup_in_root not bundle name", 1);
            return Ok(vec![]);
        }

        let results = self.lookup_in_trees(name)?;

        // If all tree results were mixfix parts, try to load a bundle with the same name.
        // If successful, add those results to the results.
        if Namer::all_mixfix(&results) {
            match self.driver.load_bundle_by_name(&name) {
                Ok(index) => {
                    match self.driver.prename_bundle(index) {
                        Ok(_) => {},
                        Err(msg) => {
                            return Err(msg)
                        },
                    }

                    let r = LookupRef { scope: Scope::Global, name };

// FIXME: should add a bundle index to all Env and Lookup refs.
// Or make the graph global.
// The returned decls have inner scopes referring to the graph in the bundle, not the current bundle.
                    match self.driver.get_bundle(index) {
                        Some(Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(Scope::Env(env_index)) = scopes.get(&id) {
                                return Namer::lookup_here_in_env(&mut self.driver.stats, &self.driver.graph, *env_index, name, &mut HashMap::new());
                                // return self.try_lookup_ref(LookupRef { scope: scope.to_here(), name }, worklist, visited, &stack.insert(r));
                            }
                            self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                        },
                        Some(Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            if let Some(Scope::Env(env_index)) = scopes.get(&id) {
                                return Namer::lookup_here_in_env(&mut self.driver.stats, &self.driver.graph, *env_index, name, &mut HashMap::new());
                                // return self.try_lookup_ref(LookupRef { scope: scope.to_here(), name }, worklist, visited, &stack.insert(r));
                            }
                            self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                        },
                        Some(Bundle::Core { root_scope, .. }) => {
                            if let Scope::Env(env_index) = &root_scope {
                                return Namer::lookup_here_in_env(&mut self.driver.stats, &self.driver.graph, *env_index, name, &mut HashMap::new());
                            }
                            self.driver.error(Located::new(Loc::no_loc(), "root scope of bundle is not an env".to_owned()))
                            // return self.try_lookup_ref(LookupRef { scope: root_scope.to_here(), name }, worklist, visited, &stack.insert(r));
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

        Ok(vec![])
    }

    #[cfg_attr(debug_assertions, trace)]
    fn lookup_in_trees(&mut self, name: Name) -> NamerResult<Decls> {
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
            trace!("naming bundle {:?} to find {:?}", index, name);

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
        // We want to call lookup_ref with EnvHere(env), but can't since we have to
        // mutably borrow driver while we're looping over the bundles and we don't want to clone
        // the bundles (which are huge).
        let stats = &mut self.driver.stats;

        let mut results = Vec::new();

        let graph = &self.driver.graph;

        for bundle in &self.driver.bundles {
            match bundle {
                Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. } => {
                    if let Some(Scope::Env(env)) = scopes.get(&id) {
                        let vs = Namer::lookup_here_in_env(stats, &graph, *env, name, &mut self.cache.lookup_cache)?;
                        results.extend(vs.iter().cloned());
                    }
                },
                Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. } => {
                    if let Some(Scope::Env(env)) = scopes.get(&id) {
                        let vs = Namer::lookup_here_in_env(stats, &graph, *env, name, &mut self.cache.lookup_cache)?;
                        results.extend(vs.iter().cloned());
                    }
                },
                // Bundle::Core { root_scope, .. } => {
                // },
                _ => {},
            }
        }

        self.driver.stats.accum("lookup_in_trees size", results.len() as u64);

        Ok(results)
    }

    #[cfg_attr(debug_assertions, trace(disable(worklist, visited, stack)))]
    fn get_inner_decls(&mut self, name: Name, outer_decls: &Decls, worklist: &mut Worklist, visited: &mut Visited, stack: &Dependencies) -> NamerResult<(bool, Decls)> {
        let mut results = Vec::new();

        let frames = outer_decls.iter().flat_map(|d| {
            match d {
                Located { loc, value: Decl::Trait { body, .. } } => body.to_vec(),
                _ => vec![]
            }
        });

        let mut all_cached = true;

        for scope in frames {
            let s = LookupRef { scope: scope.to_here(), name };
            if let Some(vs) = self.cache.lookup_cache.get(&s) {
                results.extend(vs.iter().cloned());
            }
            else {
                let vs = self.try_lookup_ref(s, worklist, visited, stack)?;
                results.extend(vs.iter().cloned());
                all_cached = false;
            }
        }

        Ok((all_cached, results))
    }

    // HACK
    // Version of lookup_here_in_scope that only works on Env environments.
    // Called from lookup_in_trees. Avoids loading other bundles which requires
    // borrowing the driver mutably. The assumption is that the root scope of a bundle
    // is always a simple Env.
    #[cfg_attr(debug_assertions, trace(disable(stats, graph, cache)))]
    fn lookup_here_in_env(stats: &mut driver::stats::Stats, graph: &ScopeGraph, env_index: EnvIndex, name: Name, cache: &mut LookupCache) -> NamerResult<Decls> {
        let r = LookupRef { scope: Scope::EnvHere(env_index), name };

        if let Some(results) = cache.get(&r) {
            stats.accum("lookup_here_in_env cache hit", 1);
            return Ok(results.clone());
        }

        let mut results = Vec::new();

        match env_index {
            EnvIndex(index) => {
                let env = graph.envs.get(index).unwrap();
                // need to clone the includes
                // before we borrow self mutably.
                let includes = env.includes.clone();

                let tmp = vec![];
                let found_here = env.decls.get(&name).unwrap_or(&tmp);

                results.extend(found_here.iter().cloned());

                for include in includes {
                    match include.to_here() {
                        Scope::EnvHere(include) if include != env_index => {
                            let vs = Namer::lookup_here_in_env(stats, graph, include, name, cache)?;
                            results.extend(vs.iter().cloned());
                        },
                        _ => {},
                    }
                }
            }
        }

        Ok(results)
    }

    fn add_stack_to_worklist(worklist: &mut Worklist, stack: &Dependencies) {
        for s in stack.iter() {
            worklist.push_back(s.clone());
        }
    }

    fn stack_contains(stack: &Dependencies, r: &LookupRef) -> bool {
        stack.contains(&r)
    }

    // fn add_stack_to_worklist(worklist: &mut Worklist, stack: &Dependencies) {
    //     if let Some(s) = stack.peek() {
    //         worklist.push_back(s.clone());
    //
    //         if let Some(rest) = stack.pop() {
    //             Namer::add_stack_to_worklist(worklist, &rest);
    //         }
    //     }
    // }
    //
    // fn stack_contains(stack: &Dependencies, r: &LookupRef) -> bool {
    //     if let Some(s) = stack.peek() {
    //         if s == r {
    //             return true;
    //         }
    //
    //         if let Some(rest) = stack.pop() {
    //             return Namer::stack_contains(&rest, r);
    //         }
    //     }
    //
    //     false
    // }

    #[cfg_attr(debug_assertions, trace(disable(worklist, visited, stack)))]
    fn try_lookup_ref(&mut self, r: LookupRef, worklist: &mut Worklist, visited: &mut Visited, stack: &Dependencies) -> NamerResult<Decls> {
        // Special case the empty scope just so we don't waste space and time with caching it.
        if r.scope == Scope::Empty {
            return Ok(vec![]);
        }

        if let Some(result) = self.cache.lookup_cache.get(&r) {
            return Ok(result.clone());
        }

        trace!("cache miss");

        let mut old_results;

        if let Some(results) = visited.get(&r) {
            // We have visited this node before.
            // Either we have a cross-edge or a back-edge.
            // If a cross-edge, the value cached here should be valid too.
            // If a back-edge, we should be returning a result <= the fixpoint value.
            // if Namer::stack_contains(stack, &r) {
            //     Namer::add_stack_to_worklist(worklist, stack);
            // }
            if Namer::stack_contains(stack, &r) {
                // Cycle in the graph. We should return the cached results.
                // But, DO NOT add ourselves to the worklist.
                // That will happen when we return from the recursive call.
                trace!("cycle");
                return Ok(results.clone());
            }

            trace!("cross edge");

            old_results = results.clone();
        }
        else {
            // Cache an initial value so we can compute the fixed point.
            visited.insert(r.clone(), vec![]);

            old_results = vec![];
        }

        trace!("forward edge");

        let name = r.name;

        let mut all_cached = true;

        let mut results = match r.scope {
            Scope::Empty => {
                Ok(vec![])
            },
            Scope::Global => {
                all_cached = false; // FIXME
                self.lookup_in_root(name, worklist, visited, &stack.insert(r))
            },
            Scope::Lookup(LookupIndex(index)) => {
                let s = self.driver.graph.lookups.get(index).unwrap();
                if let Some(vs) = self.cache.lookup_cache.get(s) {
                    trace!("cache hit");
                    let (c, rs) = self.get_inner_decls(name, &vs.clone(), worklist, visited, &stack.insert(r))?;
                    all_cached &= c;
                    Ok(rs)
                }
                else {
                    trace!("cache miss");
                    let vs = self.try_lookup_ref(s.clone(), worklist, visited, &stack.insert(r))?;
                    let (_, rs) = self.get_inner_decls(name, &vs, worklist, visited, &stack.insert(r))?;
                    all_cached = false;
                    Ok(rs)
                }
            },
            Scope::Mixfix(MixfixIndex(index)) => {
                let s = self.driver.graph.mixfixes.get(index).unwrap();
                if let Some(ts) = self.cache.tree_cache.get(s) {
                    trace!("cache hit");
                    let vs = ts.iter().flat_map(|t| Namer::mixfix_tree_to_decls(t)).collect();
                    let (c, rs) = self.get_inner_decls(name, &vs, worklist, visited, &stack.insert(r))?;
                    all_cached &= c;
                    Ok(rs)
                }
                else {
                    trace!("cache miss");
                    let ts = self.try_mixfix_ref(s.clone(), worklist, visited, &stack.insert(r))?;
                    let vs = ts.iter().flat_map(|t| Namer::mixfix_tree_to_decls(t)).collect();
                    let (_, rs) = self.get_inner_decls(name, &vs, worklist, visited, &stack.insert(r))?;
                    all_cached = false;
                    Ok(rs)
                }
            },
            Scope::EnvHere(EnvIndex(index)) => {
                let env = self.driver.graph.envs.get(index).unwrap();
                let (c, rs) = self.lookup_in_env(&r, env.clone(), name, false, false, worklist, visited, &stack.insert(r))?;
                all_cached &= c;
                Ok(rs)
            }
            Scope::EnvWithoutImports(EnvIndex(index)) => {
                let env = self.driver.graph.envs.get(index).unwrap();
                let (c, rs) = self.lookup_in_env(&r, env.clone(), name, true, false, worklist, visited, &stack.insert(r))?;
                all_cached &= c;
                Ok(rs)
            }
            Scope::Env(EnvIndex(index)) => {
                let env = self.driver.graph.envs.get(index).unwrap();
                let (c, rs) = self.lookup_in_env(&r, env.clone(), name, true, true, worklist, visited, &stack.insert(r))?;
                all_cached &= c;
                Ok(rs)
            }
        }?;

        results.append(&mut old_results.clone());
        results.sort();
        results.dedup();

        // If the value changed, we should add the node back to the worklist.
        // We don't need to add callers explicitly, because they should do the same check.
        if old_results.len() < results.len() {
            trace!("changed: {} -> {}", old_results.len(), results.len());
            trace!("changed: {:?} -> {:?}", old_results, results);
            // Namer::add_stack_to_worklist(worklist, stack);
            worklist.push_back(r);
        }
        else if old_results.len() > results.len() {
            panic!("results size should not shrink! {:?} -> {:?}", old_results, results);
        }

        // Update the cache with the new resultss.
        visited.insert(r, results.clone());

        if all_cached {
            trace!("caching!");
            trace!("worklist = {:?}", worklist);
            self.cache.lookup_cache.insert(r, results.clone());
        }

        Ok(results)
    }

    #[cfg_attr(debug_assertions, trace(disable(worklist, visited, stack)))]
    fn lookup_in_env(&mut self, r: &LookupRef, env: Env, name: Name, follow_parents: bool, follow_imports: bool, worklist: &mut Worklist, visited: &mut Visited, stack: &Dependencies) -> NamerResult<(bool, Decls)> {
        let imports = env.imports;
        let parents = env.parents;
        let includes = env.includes;
        let decls = env.decls;

        let found_here;
        let found_imports;
        let found_parents;

        let mut all_cached = true;

        let tmp = vec![]; // shut up borrow checker. The vec! has to live as long as found_here
        found_here = decls.get(&name).unwrap_or(&tmp);
        trace!("found_here = {:?}", found_here);

        if follow_imports && Namer::all_mixfix(&found_here) {
            let mut rs = Vec::new();

            let paths = Namer::import_paths(&imports, name);

            trace!("paths = {:?}", paths);

            // x here is usually the same as x, unless there's a renaming import.
            for s in paths {
                let vs = self.try_lookup_ref(s, worklist, visited, stack)?;
                rs.extend(vs.iter().cloned());

                if let None = self.cache.lookup_cache.get(&s) {
                    all_cached = false;
                }
            }

            found_imports = Namer::filter_mixfix(rs, &found_here);
        }
        else {
            found_imports = Vec::new();
        }

        trace!("found_imports = {:?}", found_imports);

        if follow_parents && Namer::all_mixfix(&found_here) && Namer::all_mixfix(&found_imports) {
            let mut rs = Vec::new();

            for parent in &parents {
                if Namer::imports_truncated(&imports, *parent) {
                    continue;
                }

                let s = LookupRef { scope: *parent, name };

                let vs = self.try_lookup_ref(s, worklist, visited, stack)?;
                rs.extend(vs.iter().cloned());

                if let None = self.cache.lookup_cache.get(&s) {
                    all_cached = false;
                }
            }

            found_parents = Namer::filter_mixfix(Namer::filter_mixfix(rs, &found_here), &found_imports);
        }
        else {
            found_parents = Vec::new();
        }

        let mut results = Vec::new();
        results.extend(found_here.iter().cloned());
        results.extend(found_imports.iter().cloned());
        results.extend(found_parents.iter().cloned());

        for include in &includes {
            let scope = if follow_parents {
                *include
            }
            else {
                include.to_here()
            };

            let s = LookupRef { scope, name };

            let vs = self.try_lookup_ref(s, worklist, visited, stack)?;
            results.extend(vs.iter().cloned());

            if let None = self.cache.lookup_cache.get(&s) {
                all_cached = false;
            }
        }

        results.sort();
        results.dedup();

        Ok((all_cached, results))
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Utility functions
    ////////////////////////////////////////////////////////////////////////////////

    // Do fast mixfix parsing. Should be called only when all the decls have a single mixfix name.
    // It just checks that the tokens match the mixfix name and builds the call tree.
    fn fast_parse(tokens: Vec<Token>, decls: &Decls, name: Name) -> Option<MixfixTree> {
        match name {
            Name::Mixfix(s) => {
                let parts = Name::decode_parts(s);

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
                let args: Trees = tokens.iter().filter_map(|t| match t {
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

    fn import_paths(imports: &Vec<Located<Import>>, x: Name) -> Vec<LookupRef> {
        let mut include = Vec::new();
        let mut include_all = Vec::new();
        let mut exclude = Vec::new();
        let mut exclude_all = Vec::new();

        for import in imports {
            match import {
                Located { loc, value: Import::None { path: scope } } => {
                    exclude_all.push(LookupRef { scope: *scope, name: x });
                }
                Located { loc, value: Import::All { path: scope } } => {
                    include_all.push(LookupRef { scope: *scope, name: x });
                }
                Located { loc, value: Import::Including { path: scope, name: y } } if *y == x => {
                    include.push(LookupRef { scope: *scope, name: x });
                }
                Located { loc, value: Import::Renaming { path: scope, name: y, rename: z } } if *z == x => {
                    include.push(LookupRef { scope: *scope, name: *y });
                }
                Located { loc, value: Import::Excluding { path: scope, name: y } } if *y == x => {
                    exclude.push(LookupRef { scope: *scope, name: x });
                }
                _ => {},
            }
        }

        let mut paths = Vec::new();
        paths.append(&mut include_all);
        for r in exclude_all {
            paths.remove_item(&r);
        }
        for r in exclude {
            paths.remove_item(&r);
        }
        paths.append(&mut include);

        // Sort the paths and remove duplicates (sorting is needed just because dedup only removes consequetive dups).
        paths.sort();
        paths.dedup();

        paths
    }

    // This resolves to the declarations in the leftmost name in
    // the mixfix tree. That is to resolve the scope of (List (Maybe Int)),
    // we just look at `List _`.
    fn mixfix_tree_to_decls(t: &MixfixTree) -> Decls {
        match t {
            MixfixTree::Apply(ref t1, ref t2) => Namer::mixfix_tree_to_decls(&**t1),
            MixfixTree::Name(ref x, ref decls) => decls.clone(),
            MixfixTree::Exp => vec![],
        }
    }

    pub fn imports_truncated(imports: &Vec<Located<Import>>, scope: Scope) -> bool {
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

    pub fn all_mixfix(decls: &Decls) -> bool {
        decls.iter().all(|d| {
            match d {
                Located { loc: _, value: Decl::MixfixPart { .. } } => true,
                _ => false,
            }
        })
    }

    fn filter_mixfix(xs: Decls, ys: &Decls) -> Decls {
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
