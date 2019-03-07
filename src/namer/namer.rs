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
use std::collections::BTreeMap;
use rpds::HashTrieSet;
use std::hash::Hash;

use trace::trace;
trace::init_depth_var!();

#[cfg(debug_assertions)]
macro_rules! trace {
    ($($arg:tt)*) => {
        DEPTH.with(|depth| {
            print!("{1:0$}[!] At {2}:{3}: ", depth.get(), " ", file!(), line!()); println!($($arg)*)
        });
    }
}

#[cfg(not(debug_assertions))]
macro_rules! trace {
    ($($arg:tt)*) => { };
}

type Trees = Vec<MixfixTree>;
type LocalRefs = Vec<LocalRef>;

type NamerResult<T> = Result<T, Located<String>>;

// TODO: make driver a trait for loading files.
pub struct Namer<'a> {
    pub driver: &'a mut driver::Driver,

    in_cycle: Vec<Attr>,
    changed: bool,
    memo: HashMap<Attr, Output>,
    computed: HashSet<Attr>,
    stack: Vec<Attr>,
}

impl<'a> Namer<'a> {
    pub fn new(driver: &'a mut driver::Driver) -> Namer<'a> {
        Namer {
            driver,

            in_cycle: Vec::new(),
            changed: false,
            memo: HashMap::new(),
            computed: HashSet::new(),
            stack: Vec::new(),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
enum Attr {
    Lookup(LookupRef),
    Mixfix(MixfixRef),
}

// TODO: we should use an associated type here I guess.
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
enum Output {
    Lookup(LocalRefs),
    Mixfix(Trees),
}

impl<'a> Namer<'a> {
    fn has_been_computed(&self, a: &Attr) -> bool {
        self.computed.contains(a)
    }

    fn get_current_value(&self, r: &Attr) -> Output {
        match self.memo.get(r) {
            Some(output) => output.clone(),
            None => {
                match r {
                    Attr::Lookup(_) => Output::Lookup(vec![]),
                    Attr::Mixfix(_) => Output::Mixfix(vec![]),
                }
            }
        }
    }

    #[cfg_attr(debug_assertions, trace)]
    pub fn do_init_lookup(&mut self, r: &LookupRef) -> NamerResult<LocalRefs> {
        self.driver.stats.accum("do_init_lookup call", 1);

        let timer = self.driver.stats.start_timer();

        // This puts the result in the cache.
        let grefs = self.do_lookup(r);

        self.driver.stats.end_timer("do_init_lookup time", timer);
        self.driver.stats.accum("do_init_lookup result size", grefs.len() as u64);

        Ok(grefs)
    }

    #[cfg_attr(debug_assertions, trace)]
    pub fn do_init_mixfix(&mut self, r: &MixfixRef) -> NamerResult<Trees> {
        self.driver.stats.accum("do_init_lookup call", 1);

        let timer = self.driver.stats.start_timer();

        // This puts the result in the cache.
        let trees = self.do_mixfix(r);

        self.driver.stats.end_timer("do_init_mixfix time", timer);
        self.driver.stats.accum("do_init_mixfix result size", trees.len() as u64);

        Ok(trees)
    }

    #[cfg_attr(debug_assertions, trace(disable(state)))]
    fn do_lookup(&mut self, t: &LookupRef) -> LocalRefs {
        match self.do_attr(&Attr::Lookup(*t)) {
            Output::Lookup(grefs) => grefs,
            _ => vec![]
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(state)))]
    fn do_mixfix(&mut self, t: &MixfixRef) -> Trees {
        match self.do_attr(&Attr::Mixfix(t.clone())) {
            Output::Mixfix(trees) => trees,
            _ => vec![]
        }
    }

    fn compute(&mut self, t: &Attr) -> Output {
        match t {
            Attr::Lookup(lookup) => Output::Lookup(self.compute_lookup(lookup)),
            Attr::Mixfix(mixfix) => Output::Mixfix(self.compute_mixfix(mixfix)),
        }
    }

    fn do_attr(&mut self, t: &Attr) -> Output {
        self.driver.stats.accum("do_attr", 1);

        if self.has_been_computed(t) {
            trace!("cache hit");
            self.driver.stats.accum("do_attr hit", 1);
            // We have previously computed this attribute occurrence so fetch it from the cache.
            return self.get_current_value(t);
        }

        self.driver.stats.accum("do_attr stack size", self.stack.len() as u64);

        if self.stack.contains(t) {
            trace!("cycle found, returning cached value");
            // If the attr is on the stack, just return the cached value.
            // This avoids infinite loops.
            self.driver.stats.accum("do_attr cycle found", 1);
            return self.get_current_value(t);
        }

        if self.in_cycle.is_empty() {
            trace!("starting cycle");

            assert_eq!(self.stack, vec![]);
            
            self.driver.stats.accum("do_attr cycle root", 1);

            // This is the first evaluation of a cyclic attribute.
            // Enter the fixed point computation.
            self.changed = true;
            self.in_cycle.push(t.clone());

            self.stack.push(t.clone());

            let mut iter = 0;

            while self.changed {
                trace!("looping cycle");

                iter += 1;

                self.changed = false;

                let prev = self.get_current_value(t);
                let next = self.compute(t);

                if prev != next {
                    trace!("{:?} changed {:?} --> {:?}", t, prev, next);
                    self.changed = true;
                    self.memo.insert(t.clone(), next);
                }

                // There's only one attribute in the cycle, so running again should not change anything.
                if self.in_cycle.len() == 1 {
                    trace!("singleton (non)cycle");
                    self.driver.stats.accum("do_attr singleton (non)cycles", 1);
                    break;
                }
            }

            trace!("exiting cycle");

            self.changed = false;

            self.driver.stats.accum("do_attr cycle size", self.in_cycle.len() as u64);
            self.driver.stats.accum("do_attr fixpoint iterations", iter);

            // Leave the fixed point computation.
            self.stack.pop();

            // Mark all attributes in the cycle as completed.
            while let Some(u) = self.in_cycle.pop() {
                self.computed.insert(u.clone());
            }

            return self.get_current_value(t);
        }

        // This node is not on the stack and we're in the middle of a fixed point computation.
        self.stack.push(t.clone());

        assert!(! self.in_cycle.is_empty());
        self.in_cycle.push(t.clone());

        let prev = self.get_current_value(t);
        let next = self.compute(t);

        self.stack.pop();

        if prev != next {
            self.memo.insert(t.clone(), next.clone());
            trace!("{:?} changed {:?} --> {:?}", t, prev, next);
            self.changed = true;
        }

        next
    }

    #[cfg_attr(debug_assertions, trace(disable(state)))]
    fn compute_lookup(&mut self, r: &LookupRef) -> LocalRefs {
        let bundle = self.driver.current_bundle.unwrap();

        match r {
            LookupRef::From { scope, name, follow_imports } => {
                // clone to shutup borrow checker.
                // we don't mutate the Decl so should be ok.
                let d = self.driver.graph.get_env(*scope).clone();

                match &d.value {
                    Decl::Root => {
                        // I think this doesn't happen.
                        let s = LookupRef::Within { scope: self.driver.graph.get_root_ref(), name: *name };
                        self.do_lookup(&s)
                    },
                    Decl::Block { parent, imports, members, .. } => {
                        let found_here = members.get(name).cloned().unwrap_or_default();

                        let mut found_imports = vec![];
                        let mut found_parent = vec![];

                        if self.all_mixfix(&found_here) {
                            if *follow_imports {
                                let paths = Namer::import_paths(imports, *name);
                                for s in paths {
                                    let mut ws = self.do_lookup(&s);
                                    found_imports.append(&mut ws);
                                }
                            }

                            if self.all_mixfix(&found_imports) {
                                let p = LookupRef::new(*parent, *name, true);
                                found_parent = self.do_lookup(&p);
                            }
                        }

                        self.filter_mixfix(&mut found_imports, &found_here);
                        self.filter_mixfix(&mut found_parent, &found_here);
                        self.filter_mixfix(&mut found_parent, &found_imports);

                        trace!("found_here = {:?}", &found_here);
                        trace!("found_imports = {:?}", &found_imports);
                        trace!("found_parent = {:?}", &found_parent);

                        let mut results = vec![];
                        results.extend(found_here.iter().cloned());
                        results.extend(found_imports.iter().cloned());
                        results.extend(found_parent.iter().cloned());
                        results.sort();
                        results.dedup();
                        results
                    },
                    Decl::Bundle { imports, members, .. } => {
                        let found_here = members.get(name).cloned().unwrap_or_default();

                        let mut found_imports = vec![];

                        if *follow_imports {
                            if self.all_mixfix(&found_here) {
                                let paths = Namer::import_paths(imports, *name);
                                for s in paths {
                                    let mut ws = self.do_lookup(&s);
                                    found_imports.append(&mut ws);
                                }
                            }
                        }

                        self.filter_mixfix(&mut found_imports, &found_here);

                        trace!("found_here = {:?}", &found_here);
                        trace!("found_imports = {:?}", &found_imports);

                        let mut results = vec![];
                        results.extend(found_here.iter().cloned());
                        results.extend(found_imports.iter().cloned());
                        results.sort();
                        results.dedup();
                        results
                    }
                    Decl::Trait { parent, imports, supers, members, .. } => {
                        let found_here = members.get(name).cloned().unwrap_or_default();

                        let mut found_imports = vec![];
                        let mut found_supers = vec![];
                        let mut found_parent = vec![];

                        if self.all_mixfix(&found_here) {
                            if *follow_imports {
                                let paths = Namer::import_paths(imports, *name);
                                for s in paths {
                                    let mut ws = self.do_lookup(&s);
                                    found_imports.append(&mut ws);
                                }
                            }

                            if self.all_mixfix(&found_imports) {
                                if *follow_imports {
                                    for p in supers {
                                        let s = LookupRef::Within { scope: *p, name: *name };
                                        let mut ws = self.do_lookup(&s);
                                        found_supers.append(&mut ws);
                                    }
                                }

                                if self.all_mixfix(&found_supers) {
                                    let p = LookupRef::new(*parent, *name, true);
                                    found_parent = self.do_lookup(&p);
                                }
                            }
                        }

                        self.filter_mixfix(&mut found_imports, &found_here);
                        self.filter_mixfix(&mut found_supers, &found_here);
                        self.filter_mixfix(&mut found_supers, &found_imports);
                        self.filter_mixfix(&mut found_parent, &found_here);
                        self.filter_mixfix(&mut found_parent, &found_imports);
                        self.filter_mixfix(&mut found_parent, &found_supers);

                        trace!("found_here = {:?}", &found_here);
                        trace!("found_imports = {:?}", &found_imports);
                        trace!("found_supers = {:?}", &found_supers);
                        trace!("found_parent = {:?}", &found_parent);

                        let mut results = vec![];
                        results.extend(found_here.iter().cloned());
                        results.extend(found_imports.iter().cloned());
                        results.extend(found_supers.iter().cloned());
                        results.extend(found_parent.iter().cloned());
                        results.sort();
                        results.dedup();
                        results
                    },
                    Decl::Fun { parent, .. } => {
                        let p = LookupRef::new(*parent, *name, true);
                        self.do_lookup(&p)
                    },
                    Decl::Val { parent, .. } => {
                        let p = LookupRef::new(*parent, *name, true);
                        self.do_lookup(&p)
                    },
                    Decl::Var { parent, .. } => {
                        let p = LookupRef::new(*parent, *name, true);
                        self.do_lookup(&p)
                    },
                    Decl::MixfixPart { .. } => {
                        vec![]
                    },
                }
            },
            LookupRef::Within { scope, name } => {
                match scope {
                    Ref::Resolved(gref) => {
                        let d = self.driver.graph.get_env(*gref).clone();

                        match &d.value {
                            Decl::Root => {
                                match self.lookup_in_root(*name) {
                                    Ok(grefs) => {
                                        grefs
                                    },
                                    _ => {
                                        vec![]
                                    },
                                }
                            },
                            Decl::Block { parent, imports, members, .. } => {
                                members.get(name).cloned().unwrap_or_default()
                            },
                            Decl::Bundle { imports, members, .. } => {
                                members.get(name).cloned().unwrap_or_default()
                            },
                            Decl::Trait { parent, imports, supers, members, .. } => {
                                let mut vs = members.get(name).cloned().unwrap_or_default();

                                for p in supers {
                                    let s = LookupRef::Within { scope: *p, name: *name };
                                    let mut ws = self.do_lookup(&s);
                                    vs.append(&mut ws);
                                }

                                vs
                            },
                            Decl::Fun { parent, .. } => {
                                vec![]
                            },
                            Decl::Val { parent, .. } => {
                                vec![]
                            },
                            Decl::Var { parent, .. } => {
                                vec![]
                            },
                            Decl::MixfixPart { .. } => {
                                vec![]
                            },
                        }
                    },
                    Ref::Lookup(index) => {
                        let mut vs = vec![];
                        let s = self.driver.graph.get_lookup(index);
                        let ws = self.do_lookup(&s);
                        for gref in ws {
                            let p = LookupRef::Within { scope: Ref::Resolved(gref), name: *name };
                            let mut ws = self.do_lookup(&p);
                            vs.append(&mut ws);
                        }
                        vs
                    },
                    Ref::Mixfix(index) => {
                        let mut vs = vec![];
                        let s = self.driver.graph.get_mixfix(index);
                        let ts = self.do_mixfix(&s);
                        let grefs: Vec<LocalRef> = ts.iter().flat_map(|t| Namer::mixfix_tree_to_decls(t)).collect();
                        for gref in grefs {
                            let p = LookupRef::Within { scope: Ref::Resolved(gref), name: *name };
                            let mut ws = self.do_lookup(&p);
                            vs.append(&mut ws);
                        }
                        vs
                    }
                }
            },
        }
    }

    #[cfg_attr(debug_assertions, trace)]
    pub fn lookup(&mut self, r: &LookupRef) -> NamerResult<LocalRefs> {
        self.do_init_lookup(r)
    }

    #[cfg_attr(debug_assertions, trace)]
    pub fn parse_mixfix(&mut self, m: &MixfixRef) -> NamerResult<Trees> {
        self.do_init_mixfix(m)
    }

    fn filter_mixfix(&self, xs: &mut LocalRefs, ys: &[LocalRef]) {
        xs.drain_filter(|x| ! {
            match self.driver.graph.get_env(*x) {
                Located { loc, value: Decl::MixfixPart { full: fullx, .. } } =>
                    ys.iter().all(|y| {
                        match self.driver.graph.get_env(*y) {
                            Located { loc, value: Decl::MixfixPart { full: fully, .. } } => fullx != fully,
                            _ => true,
                        }
                    }),
                _ => true,
            }
        });
    }

    pub(super) fn all_mixfix(&self, grefs: &[LocalRef]) -> bool {
        grefs.iter().all(|gref| {
            match self.driver.graph.get_env(*gref) {
                Located { loc, value: Decl::MixfixPart { .. } } => true,
                _ => false,
            }
        })
    }

    #[cfg_attr(debug_assertions, trace(disable(state)))]
    fn compute_mixfix(&mut self, r: &MixfixRef) -> Trees {
        self.driver.stats.accum("mixfix expression size", r.parts.len() as u64);

        let mut refs: Vec<Option<LookupRef>> = vec![];

        for part in &r.parts {
            match part.name_ref {
                Some(LookupIndex(index)) => {
                    let r = self.driver.graph.lookups.get(index).cloned();
                    refs.push(r);
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
                    let grefs = self.do_lookup(&r);

                    if ! grefs.is_empty() && self.all_mixfix(&grefs) {
                        match &r.name() {
                            Name::Id(x) => {
                                tokens.push(Token::Name(Part::Id(x.to_owned()), grefs.clone()));
                                has_name = true;
                            },
                            Name::Op(x) => {
                                tokens.push(Token::Name(Part::Op(x.to_owned()), grefs.clone()));
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
            // This will reduce the number of decls to consider during mixfix parsing and therefore reduce the size of the grammar and therefore speed up the mixfix parser.
            let mut parts_in_tokens = HashSet::new();

            for t in &tokens {
                if let Token::Name(x, _) = t {
                    parts_in_tokens.insert(x);
                }
            }

            let bundle = self.driver.current_bundle.unwrap();

            let mut grefs: LocalRefs = Vec::new();
            let mut part_grefs: LocalRefs = Vec::new();

            for t in &tokens {
                if let Token::Name(_, xrefs) = t {
                    for xref in xrefs {
                        part_grefs.push(xref.clone());
                        if let Located { loc, value: Decl::MixfixPart { full: Name::Mixfix(s), orig, .. } } = self.driver.graph.get_env(*xref) {
                            let parts = Name::decode_parts(*s);
                            if parts.iter().all(|part| part == &Part::Placeholder || parts_in_tokens.contains(part)) {
                                grefs.push(*orig);
                            }
                        }
                    }
                }
            }

            trace!("parts {:?}", r.parts);
            trace!("tokens {:?}", tokens);
            trace!("decls {:?}", grefs);

            self.driver.stats.accum("mixfix decls size", grefs.len() as u64);

            // The names in the expression did not resolve into anything we can possibly parse.
            // Return early.
            if grefs.is_empty() {
                return vec![];
            }

            let mut trees = {
                // if all decls have the same name, we can use fast parsing.
                let pairs: Vec<(LocalRef, Located<Decl>)> = grefs.iter().map(|gref| (*gref, self.driver.graph.get_env(*gref).clone())).collect();
                let part_pairs: Vec<(LocalRef, Located<Decl>)> = part_grefs.iter().map(|gref| (*gref, self.driver.graph.get_env(*gref).clone())).collect();

                let mut names: Vec<Name> = pairs.iter().map(|(gref, d)| d.value.name()).collect();
                names.sort();
                names.dedup();

                if names.len() == 1 {
                    self.driver.stats.accum("mixfix fast parse", 1);

                    let timer = self.driver.stats.start_timer();

                    let trees = match Namer::fast_parse(tokens, &grefs, *names.first().unwrap()) {
                        Some(t) => vec![t],
                        None => vec![],
                    };

                    self.driver.stats.end_timer("fast mixfix parsing time", timer);

                    trees
                }
                else {
                    self.driver.stats.accum("mixfix Earley parse decls size", grefs.len() as u64);

                    let timer = self.driver.stats.start_timer();

                    let trees = Earley::new(&pairs, &part_pairs).parse(&tokens);

                    self.driver.stats.end_timer("earley parsing time", timer);

                    trees
                }
            };

            results.append(&mut trees);
        }

        results
    }

    #[cfg_attr(debug_assertions, trace(disable(state)))]
    fn lookup_in_root(&mut self, name: Name) -> NamerResult<LocalRefs> {
        self.driver.stats.accum("lookup_in_root", 1);

        // we only search other bundles for legal bundle names
        if ! name.is_bundle_name() {
            self.driver.stats.accum("lookup_in_root not bundle name", 1);
            return Ok(vec![]);
        }

        let results = self.lookup_in_trees(name)?;

        // If all tree results were mixfix parts, try to load a bundle with the same name.
        // If successful, add those results to the results.
        if self.all_mixfix(&results) {
            match self.driver.load_bundle_by_name(name) {
                Ok(index) => {
                    match self.driver.prename_bundle(index) {
                        Ok(_) => {},
                        Err(msg) => {
                            return Err(msg)
                        },
                    }

                    let r = LookupRef::as_member(self.driver.graph.get_root_ref(), name);

// FIXME: should add a bundle index to all Env and Lookup refs.
// Or make the graph global.
// The returned decls have inner scopes referring to the graph in the bundle, not the current bundle.
                    match self.driver.get_bundle(index) {
                        Some(Bundle::Prenamed { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            let graph = &self.driver.graph;
                            if let Some(Located { loc, value: Decl::Bundle { members, .. } }) = scopes.get(&id).map(|i| graph.get_env(*i)) {
                                return Ok(self.get_members(name, &members))
                            }
                            self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                        },
                        Some(Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. }) => {
                            let graph = &self.driver.graph;
                            if let Some(Located { loc, value: Decl::Bundle { members, .. } }) = scopes.get(&id).map(|i| graph.get_env(*i)) {
                                return Ok(self.get_members(name, &members))
                            }
                            self.driver.error(Located::new(loc, "root scope of bundle not found".to_owned()))
                        },
                        Some(Bundle::Core { root_scope, .. }) => {
                            let graph = &self.driver.graph;
                            if let Some(Located { loc, value: Decl::Bundle { members, .. } }) = Some(&root_scope).map(|i| graph.get_env(*i)) {
                                return Ok(self.get_members(name, &members))
                            }
                            self.driver.error(Located::new(Loc::no_loc(), "root scope of bundle is not an env".to_owned()))
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

    #[cfg_attr(debug_assertions, trace)]
    fn lookup_in_trees(&mut self, name: Name) -> NamerResult<LocalRefs> {
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
                        panic!("found un-named bundle after naming");
                    },
                    Bundle::Parsed { .. } => {
                        panic!("found un-named bundle after naming");
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
                    if let Some(Located { loc, value: Decl::Bundle { members, .. } }) = scopes.get(&id).map(|i| graph.get_env(*i)) {
                        let vs = self.get_members(name, &members);
                        results.extend(vs.iter().cloned());
                    }
                },
                Bundle::Named { tree: Located { loc, value: trees::Root::Bundle { id, .. } }, scopes, .. } => {
                    if let Some(Located { loc, value: Decl::Bundle { members, .. } }) = scopes.get(&id).map(|i| graph.get_env(*i)) {
                        let vs = self.get_members(name, &members);
                        results.extend(vs.iter().cloned());
                    }
                },
                Bundle::Core { root_scope, .. } => {
                    if let Some(Located { loc, value: Decl::Bundle { members, .. } }) = Some(root_scope).map(|i| graph.get_env(*i)) {
                        let vs = self.get_members(name, &members);
                        results.extend(vs.iter().cloned());
                    }
                },
                _ => {},
            }
        }

        self.driver.stats.accum("lookup_in_trees size", results.len() as u64);

        Ok(results)
    }

    fn get_members(&self, name: Name, members: &BTreeMap<Name, Vec<LocalRef>>) -> LocalRefs {
        let bundle = self.driver.current_bundle.unwrap();
        members.get(&name).cloned().unwrap_or_default()
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Utility functions
    ////////////////////////////////////////////////////////////////////////////////

    // Do fast mixfix parsing. Should be called only when all the decls have a single mixfix name.
    // It just checks that the tokens match the mixfix name and builds the call tree.
    fn fast_parse(tokens: Vec<Token>, grefs: &[LocalRef], name: Name) -> Option<MixfixTree> {
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

                Some(MixfixTree::make_call(MixfixTree::Name(name, grefs.to_vec()), &args))
            },
            _ => {
                None
            },
        }
    }

    fn import_paths(imports: &[Located<Import>], x: Name) -> Vec<LookupRef> {
        let mut include = Vec::new();
        let mut include_all = Vec::new();
        let mut exclude = Vec::new();
        let mut exclude_all = Vec::new();

        for import in imports {
            match import {
                Located { loc, value: Import::None { path: scope } } => {
                    exclude_all.push(LookupRef::as_member(*scope, x));
                }
                Located { loc, value: Import::All { path: scope } } => {
                    include_all.push(LookupRef::as_member(*scope, x));
                }
                Located { loc, value: Import::Including { path: scope, name: y } } if *y == x => {
                    include.push(LookupRef::as_member(*scope, x));
                }
                Located { loc, value: Import::Renaming { path: scope, name: y, rename: z } } if *z == x => {
                    include.push(LookupRef::as_member(*scope, *y));
                }
                Located { loc, value: Import::Excluding { path: scope, name: y } } if *y == x => {
                    exclude.push(LookupRef::as_member(*scope, x));
                }
                _ => {},
            }
        }

        // sort and dedup because remove_item just removes the first occurrence.
        // (sorting is needed just because dedup only removes consequetive dups).
        include_all.sort();
        include_all.dedup();

        for r in exclude_all {
            include_all.remove_item(&r);
        }
        for r in exclude {
            include_all.remove_item(&r);
        }

        let mut paths = Vec::new();
        paths.append(&mut include_all);
        paths.append(&mut include);

        // Dedup the paths 
        paths.sort();
        paths.dedup();

        paths
    }

    // This resolves to the declarations in the leftmost name in
    // the mixfix tree. That is to resolve the scope of (List (Maybe Int)),
    // we just look at `List _`.
    fn mixfix_tree_to_decls(t: &MixfixTree) -> LocalRefs {
        match t {
            MixfixTree::Apply(ref t1, ref t2) => Namer::mixfix_tree_to_decls(&**t1),
            MixfixTree::Name(ref x, ref decls) => decls.clone(),
            MixfixTree::Exp => vec![],
        }
    }

    pub fn imports_truncated(imports: &[Located<Import>], scope: Ref) -> bool {
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
}
