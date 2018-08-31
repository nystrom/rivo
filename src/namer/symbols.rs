use syntax::names::Name;
use syntax::names::Part;
use syntax::loc::*;
use syntax::trees::Assoc;
use syntax::trees::CallingConv;
use syntax::trees::CallingMode;

// During naming we form a graph of scopes, refs, and declarations.
// Rather than represent the graph using references, we use
// vector indices. This simplifies the memory management considerably.
// We wrap the indexes some structs to improve type safety.

use namer::namer::{LookupIndex, LookupHereIndex, MixfixIndex, EnvIndex};

// need to implement hash for breadcrumbs to work.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Scope {
    Empty,
    Global,
    Lookup(LookupIndex),
    LookupHere(LookupHereIndex),
    Mixfix(MixfixIndex),
    Env(EnvIndex),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    pub decls: Vec<Decl>,
    pub imports: Vec<Located<Import>>,
    pub parents: Vec<Scope>,
    pub includes: Vec<Scope>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Prio(usize);

impl Env {
    pub fn new() -> Env {
        Env {
            decls: vec![],
            imports: vec![],
            parents: vec![],
            includes: vec![],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Import {
    All { path: Scope },
    None { path: Scope },
    Including { path: Scope, name: Name },
    Excluding { path: Scope, name: Name },
    Renaming { path: Scope, name: Name, rename: Name },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Trait {
        scope: Scope,
        name: Name,
        param_assocs: Vec<Assoc>,
        param_convs: Vec<CallingConv>,
        param_modes: Vec<CallingMode>,
        ret_mode: CallingMode,
        ret_conv: CallingConv,
        prio: Prio,
        body: Vec<Scope>,
    },

    Fun {
        scope: Scope,
        name: Name,
        param_assocs: Vec<Assoc>,
        param_convs: Vec<CallingConv>,
        param_modes: Vec<CallingMode>,
        ret_mode: CallingMode,
        ret_conv: CallingConv,
        prio: Prio,
    },

    Val {
        scope: Scope,
        name: Name,
    },

    Var {
        scope: Scope,
        name: Name,
    },

    MixfixPart {
        name: Name,
        index: usize,
        full: Name,
        orig: Box<Decl>
    },
}


impl Decl {
    pub fn name(&self) -> Name {
        match *self {
            Decl::Trait { ref name, .. } => name.clone(),
            Decl::Fun { ref name, .. } => name.clone(),
            Decl::Val { ref name, .. } => name.clone(),
            Decl::Var { ref name, .. } => name.clone(),
            Decl::MixfixPart { ref name, .. } => name.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LookupRef {
    pub scope: Scope,
    pub name: Name,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LookupHereRef {
    pub scope: Scope,
    pub name: Name,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MixfixRef {
    pub parts: Vec<MixfixPart>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MixfixPart {
    pub name_ref: Option<LookupRef>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MixfixTree {
    Name(Name, Vec<Located<Decl>>),
    Apply(Box<MixfixTree>, Box<MixfixTree>),
    Exp,
}

// 1 + 2 + 3 -->
// ((`_ + _` ((`_ + _` 1) 2)) 3)
// the structure is
// MixfixTreeApply(
//   MixfixTreeApply(
//     MixfixTreeName(`_ + _`),
//     MixfixTreeApply(
//       MixfixTreeApply(
//         MixfixTreeName(`_ + _`),
//         MixfixExp()),
//       MixfixExp())),
// MixfixExp())
// We can apply this tree to a list of expressions to create the
// correctly nested apply expression. See applyMixfixTree in Namer.
