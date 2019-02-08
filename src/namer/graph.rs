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
pub struct EnvIndex(pub usize);

#[derive(Clone, Debug)]
pub struct ScopeGraph {
    // Since lookups refer to scopes, which may in turn contain other lookups, we
    // store the refs in the graph and refer to the them by index.
    pub(super) lookups: Vec<LookupRef>,
    pub(super) mixfixes: Vec<MixfixRef>,
    pub(super) pre_resolved: Vec<Option<Vec<Located<Decl>>>>,

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
            pre_resolved: Vec::new(),
            envs: Vec::new(),
        }
    }

    pub fn new_env(&mut self) -> Scope {
        let index = self.envs.len();
        self.envs.push(Env {
            index: EnvIndex(index),
            decls: BTreeMap::new(),
            imports: Vec::new(),
            parents: Vec::new(),
            includes: Vec::new(),
            path: None,
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
    pub fn get_pre_resolved(&self, index: &LookupIndex) -> Option<&Vec<Located<Decl>>> {
        match index {
            LookupIndex(i) => {
                match self.pre_resolved.get(*i) {
                    Some(Some(vs)) => Some(vs),
                    Some(None) => None,
                    None => None,
                }
            }
        }
    }

    pub fn resolve(&mut self, index: &LookupIndex, decl: &Located<Decl>) {
        match index {
            LookupIndex(i) => {
                match self.pre_resolved.get_mut(*i) {
                    Some(Some(vs)) => {
                        vs.push(decl.clone());
                    },
                    Some(None) | None => {
                        if *i >= self.pre_resolved.len() {
                            self.pre_resolved.resize_default(*i+1);
                        }
                        let vs = vec![decl.clone()];
                        self.pre_resolved[*i] = Some(vs);
                    },
                }
            }
        }
    }

    pub fn get_scope_of_lookup(&self, r: LookupIndex) -> Scope {
        Scope::Lookup(r)
    }

    pub fn get_scope_of_mixfix(&self, r: MixfixIndex) -> Scope {
        Scope::Mixfix(r)
    }

    pub fn set_path(&mut self, scope: Scope, path: Located<StablePath>) {
        match scope.to_here() {
            Scope::EnvHere(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.path = Some(path)
                }
            }
            _ => {},
        }
    }

    pub fn get_path(&self, scope: Scope) -> Option<Located<StablePath>> {
        match scope.to_here() {
            Scope::EnvHere(EnvIndex(index)) => {
                if let Some(ref env) = self.envs.get(index) {
                    return env.path.clone()
                }
            }
            _ => {},
        }
        None
    }

    pub fn declare(&mut self, scope: Scope, decl: &Located<Decl>) {
        match scope.to_here() {
            Scope::EnvHere(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    match env.decls.get_mut(&decl.name()) {
                        Some(decls) => {
                            decls.push(decl.clone());
                        },
                        None => {
                            env.decls.insert(decl.name(), vec![decl.clone()]);
                        }
                    }
                }
            },
            _ => {},
        }
    }

    pub fn set_parent(&mut self, scope: Scope, parent: Scope) {
        if parent == Scope::Empty {
            return;
        }
        match scope.to_here() {
            Scope::EnvHere(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.parents.push(parent)
                }
            },
            _ => {},
        }
    }

    pub fn import(&mut self, scope: Scope, import: &Located<Import>) {
        if import.value.path() == Scope::Empty {
            return;
        }
        match scope.to_here() {
            Scope::EnvHere(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.imports.push(import.clone())
                }
            },
            _ => {},
        }
    }

    pub fn include(&mut self, scope: Scope, include: Scope) {
        if include == Scope::Empty {
            return;
        }
        match scope.to_here() {
            Scope::EnvHere(EnvIndex(index)) => {
                if let Some(ref mut env) = self.envs.get_mut(index) {
                    env.includes.push(include)
                }
            },
            _ => {},
        }
    }

    pub fn lookup(&mut self, scope: Scope, name: Name) -> LookupIndex {
        // SLOW HACK: we should have a table for this, but waste time rather than space.
        // Previous lookups in the same scope are more likely to be toward the end of the vec, so search backward.
        let r = LookupRef::new(scope, name);

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

    pub fn select_frame(&mut self, scope: Scope, name: Name) -> Scope {
        let r = self.lookup(scope, name);
        self.get_scope_of_lookup(r)
    }

    pub fn new_child_scope(&mut self, parent: Scope) -> Scope {
        let env = self.new_env();
        self.set_parent(env, parent);
        env
    }
}
