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

#[cfg(debug_assertions)]
#[allow(non_upper_case_globals)]
static mut depth: u32 = 0;

type Crumbs = HashTrieSet<Scope>;

// The vectors in this data structure are indexed by the *Index types.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LookupIndex(pub(super) usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MixfixIndex(pub(super) usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnvIndex(pub usize);

#[derive(Clone, Debug)]
pub struct ScopeGraph {
    // Since lookups refer to scopes, which may in turn contain other lookups, we
    // store the refs in the graph and refer to the them by index.
    pub(super) lookups: Vec<LookupRef>,
    pub(super) mixfixes: Vec<MixfixRef>,

    // This is the vector of environments (mutable scopes).
    pub(super) envs: Vec<Env>,
}

// To build the graph, essentially we have a little algebra of scoping operations
// that constructs the graph.

#[cfg_attr(debug_assertions, trace)]
impl ScopeGraph {
    pub fn new() -> ScopeGraph {
        ScopeGraph {
            lookups: Vec::new(),
            mixfixes: Vec::new(),
            envs: Vec::new(),
        }
    }

    pub fn new_env(&mut self) -> Scope {
        let index = self.envs.len();
        self.envs.push(Env {
            index: EnvIndex(index),
            decls: Vec::new(),
            imports: Vec::new(),
            parents: Vec::new(),
            includes: Vec::new(),
        });
        Scope::Env(EnvIndex(index))
    }

    pub fn get_lookup(&self, index: &LookupIndex) -> LookupRef {
        match index {
            LookupIndex(i) => self.lookups.get(*i).unwrap().clone()
        }
    }
    pub fn get_mixfix(&self, index: &MixfixIndex) -> MixfixRef {
        match index {
            MixfixIndex(i) => self.mixfixes.get(*i).unwrap().clone()
        }
    }
    pub fn get_env(&self, index: &EnvIndex) -> Env {
        match index {
            EnvIndex(i) => self.envs.get(*i).unwrap().clone()
        }
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
        let r = LookupRef::new(scope, name);
        self.lookups.push(r);
        LookupIndex(index)
    }
    pub fn lookup_here(&mut self, scope: Scope, name: Name) -> LookupIndex {
        let index = self.lookups.len();
        let r = LookupRef::new_here(scope, name);
        self.lookups.push(r);
        LookupIndex(index)
    }
    pub fn parse_mixfix(&mut self, parts: Vec<MixfixPart>) -> MixfixIndex {
        let index = self.mixfixes.len();
        let r = MixfixRef { parts };
        self.mixfixes.push(r);
        MixfixIndex(index)
    }
    pub fn select_frame(&mut self, scope: Scope, name: Name) -> Scope {
        let r = self.lookup_here(scope, name);
        self.get_scope_of_lookup(r)
    }
    pub fn new_child_scope(&mut self, parent: Scope) -> Scope {
        let env = self.new_env();
        self.set_parent(env, parent);
        env
    }
}
