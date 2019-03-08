// ScopeGraph maintains the scope graph.

use syntax::loc::*;
use syntax::names::*;
use syntax::trees;

use namer::symbols::*;

use driver;
use driver::*;
use driver::bundle::*;

use std::collections::HashMap;
use std::collections::BTreeMap;

use trace::trace;
trace::init_depth_var!();

// The vectors in this data structure are indexed by the *Index types.
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct LookupIndex(pub(super) usize);

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct MixfixIndex(pub(super) usize);

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct EnvIndex(pub(super) usize);

#[derive(Clone, Debug)]
pub struct ScopeGraph {
    // TODO: get rid of these. We can just use LookupRef and MixfixRef.

    // Since lookups refer to scopes, which may in turn contain other lookups, we
    // store the refs in the graph and refer to the them by index.
    pub(super) lookups: Vec<LookupRef>,
    pub(super) mixfixes: Vec<MixfixRef>,

    // This is the vector of environments (mutable scopes).
    // Indexed by LocalRef.
    pub(super) envs: Vec<Located<Decl>>,
}

// To build the graph, essentially we have a little algebra of scoping operations
// that constructs the graph.

#[cfg_attr(debug_assertions, trace)]
impl ScopeGraph {
    pub fn new() -> ScopeGraph {
        ScopeGraph {
            lookups: Vec::new(),
            mixfixes: Vec::new(),
            envs: vec![Located::new(Loc::no_loc(), Decl::Root)],
        }
    }

    pub fn get_root_env(&self) -> LocalRef {
        assert!(! self.envs.is_empty());
        assert!(self.envs.first().unwrap().value == Decl::Root); 
        EnvIndex(0)
    }

    pub fn add_env(&mut self, d: Located<Decl>) -> LocalRef {
        let index = self.envs.len();
        self.envs.push(d);
        EnvIndex(index)
    }

    pub fn get_lookup(&self, index: &LookupIndex) -> LookupRef {
        match index {
            LookupIndex(i) => self.lookups[*i]
        }
    }

    pub fn get_mixfix(&self, index: &MixfixIndex) -> MixfixRef {
        match index {
            MixfixIndex(i) => self.mixfixes[*i].clone()
        }
    }

    pub fn get_env(&self, index: EnvIndex) -> &Located<Decl> {
        &self.envs[index.0]
    }

    pub fn get_root_ref(&self) -> Ref {
        self.get_root_env().to_ref()
    }

    pub fn get_scope_of_lookup(&self, r: LookupIndex) -> Ref {
        Ref::Lookup(r)
    }

    pub fn get_scope_of_mixfix(&self, r: MixfixIndex) -> Ref {
        Ref::Mixfix(r)
    }

    pub fn declare(&mut self, scope: LocalRef, name: Name, decl: LocalRef) {
        if let Some(ref mut env) = self.envs.get_mut(scope.0) {
            match &mut env.value {
                Decl::Bundle { ref mut members, .. } => {
                    match members.get_mut(&name) {
                        Some(decls) => { decls.push(decl); },
                        None => { members.insert(name, vec![decl]); },
                    }
                },
                Decl::Block { ref mut members, .. } => {
                    match members.get_mut(&name) {
                        Some(decls) => { decls.push(decl); },
                        None => { members.insert(name, vec![decl]); },
                    }
                },
                Decl::Trait { ref mut members, .. } => {
                    match members.get_mut(&name) {
                        Some(decls) => { decls.push(decl); },
                        None => { members.insert(name, vec![decl]); },
                    }
                },
                d => unimplemented!("{:?}", d),
            }
        }
    }

    pub fn add_super(&mut self, scope: LocalRef, sup: Ref) {
        if let Some(ref mut env) = self.envs.get_mut(scope.0) {
            match &mut env.value {
                Decl::Trait { ref mut supers, .. } => supers.push(sup),
                _ => unimplemented!(),
            }
        }
    }

    pub fn import(&mut self, scope: LocalRef, import: Located<Import>) {
        if let Some(ref mut env) = self.envs.get_mut(scope.0) {
            match &mut env.value {
                Decl::Bundle { ref mut imports, .. } => {
                    imports.push(import)
                },
                Decl::Block { ref mut imports, .. } => {
                    imports.push(import)
                },
                Decl::Trait { ref mut imports, .. } => {
                    imports.push(import)
                },
                _ => unimplemented!(),
            }
        }
    }

    pub fn lookup_from(&mut self, scope: LocalRef, name: Name, follow_imports: bool) -> LookupIndex {
        // SLOW HACK: we should have a table for this, but waste time rather than space.
        // Previous lookups in the same scope are more likely to be toward the end of the vec, so search backward.
        let r = LookupRef::new(scope, name, follow_imports);

        for (i, s) in self.lookups.iter().enumerate().rev() {
            if s == &r {
                return LookupIndex(i)
            }
        }

        let index = self.lookups.len();
        self.lookups.push(r);
        LookupIndex(index)
    }

    pub fn lookup_inside(&mut self, scope: Ref, name: Name) -> LookupIndex {
        // SLOW HACK: we should have a table for this, but waste time rather than space.
        // Previous lookups in the same scope are more likely to be toward the end of the vec, so search backward.
        // TODO: use a map rather than a vec.
        let r = LookupRef::as_member(scope, name);

        for (i, s) in self.lookups.iter().enumerate().rev() {
            if s == &r {
                return LookupIndex(i)
            }
        }

        let index = self.lookups.len();
        self.lookups.push(r);
        LookupIndex(index)
    }

    pub fn parse_mixfix(&mut self, parts: Vec<MixfixPart>) -> MixfixIndex {
        // SLOW HACK: we should have a table for this, but waste time rather than space.
        let r = MixfixRef { parts };

        for (i, s) in self.mixfixes.iter().enumerate().rev() {
            if s == &r {
                return MixfixIndex(i)
            }
        }

        let index = self.mixfixes.len();
        self.mixfixes.push(r);
        MixfixIndex(index)
    }
}
