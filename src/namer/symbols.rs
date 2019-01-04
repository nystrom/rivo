use syntax::names::Name;
use syntax::names::Part;
use syntax::loc::*;
use syntax::trees::Assoc;
use syntax::trees::CallingConv;
use syntax::trees::CallingMode;
use syntax::trees::ParamAttr;
use syntax::trees::Lit;
use syntax::trees::Attr;

use std::collections::BTreeMap;

// During naming we form a graph of scopes, refs, and declarations.
// Rather than represent the graph using references, we use
// vector indices. This simplifies the memory management considerably.
// We wrap the indexes some structs to improve type safety.

use namer::graph::{LookupIndex, MixfixIndex, EnvIndex};

// need to implement hash for breadcrumbs to work.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Scope {
    Empty,
    Global,
    Lookup(LookupIndex),   // should be a LookupRef, but LookupRef contains a Scope and we can't have a cyclic data structure
    Mixfix(MixfixIndex),
    Env(EnvIndex),
    EnvHere(EnvIndex), // just like env but we don't search imports or parents
    EnvWithoutImports(EnvIndex), // just like env but we don't search imports
}

impl Scope {
    pub fn to_here(&self) -> Scope {
        match *self {
            Scope::Env(i) => Scope::EnvHere(i),
            Scope::EnvWithoutImports(i) => Scope::EnvHere(i),
            scope => scope,
        }
    }
    pub fn without_imports(&self) -> Scope {
        match *self {
            Scope::Env(i) => Scope::EnvWithoutImports(i),
            Scope::EnvWithoutImports(i) => Scope::EnvWithoutImports(i),
            scope => scope,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    pub index: EnvIndex, // for easier debugging
    pub decls: BTreeMap<Name, Vec<Located<Decl>>>,  // FIXME: use a BTreeMap keyed on name.
    pub imports: Vec<Located<Import>>,
    pub parents: Vec<Scope>,
    pub includes: Vec<Scope>,
    pub path: Option<Located<StablePath>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Prio(pub usize);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Import {
    All { path: Scope },
    None { path: Scope },
    Here { path: Scope },
    Including { path: Scope, name: Name },
    Excluding { path: Scope, name: Name },
    Renaming { path: Scope, name: Name, rename: Name },
}

impl Import {
    pub fn path(&self) -> Scope {
        match self {
            Import::All { path } => *path,
            Import::None { path } => *path,
            Import::Here { path } => *path,
            Import::Including { path, .. } => *path,
            Import::Excluding { path, .. } => *path,
            Import::Renaming { path, .. } => *path,
        }
    }
}

// A stable path. These can be evaluated at compile time.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum StablePath {
    Root,
    Fresh,
    Unstable,
    Lit { lit: Lit },
    Select { outer: Box<Located<StablePath>>, name: Name },
    Apply { fun: Box<Located<StablePath>>, arg: Box<Located<StablePath>> },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Decl {
    Trait {
        scope: Scope,
        name: Name,
        params: Vec<ParamAttr>,   // FIXME merge into one vec.
        ret: ParamAttr,
        prio: Prio,
        body: Vec<Scope>,
    },

    Fun {
        scope: Scope,
        name: Name,
        params: Vec<ParamAttr>,   // FIXME merge into one vec.
        ret: ParamAttr,
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
        match self {
            Decl::Trait { name, .. } => name.clone(),
            Decl::Fun { name, .. } => name.clone(),
            Decl::Val { name, .. } => name.clone(),
            Decl::Var { name, .. } => name.clone(),
            Decl::MixfixPart { name, .. } => name.clone(),
        }
    }

    pub fn assoc(&self) -> Option<usize> {
        match self {
            Decl::Trait { params, .. } => {
                for (i, p) in params.iter().enumerate() {
                    if p.assoc == Assoc::Assoc {
                        return Some(i)
                    }
                }
            },
            Decl::Fun { params, .. } => {
                for (i, p) in params.iter().enumerate() {
                    if p.assoc == Assoc::Assoc {
                        return Some(i)
                    }
                }
            },
            _ => {},
        }

        None
    }

    pub fn scope(&self) -> Scope {
        match self {
            Decl::Trait { scope, .. } => *scope,
            Decl::Fun { scope, .. } => *scope,
            Decl::Val { scope, .. } => *scope,
            Decl::Var { scope, .. } => *scope,
            _ => Scope::Empty,
        }
    }

    pub fn prio(&self) -> Prio {
        match self {
            Decl::Trait { prio, .. } => *prio,
            Decl::Fun { prio, .. } => *prio,
            _ => Prio(0),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LookupRef {
    pub scope: Scope,
    pub name: Name,
}

impl LookupRef {
    pub fn new(scope: Scope, name: Name) -> LookupRef {
        LookupRef {
            scope, name
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MixfixRef {
    pub parts: Vec<MixfixPart>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MixfixPart {
    pub name_ref: Option<LookupIndex>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum MixfixTree {
    Name(Name, Vec<Located<Decl>>),
    Apply(Box<MixfixTree>, Box<MixfixTree>),
    Exp,
}

use std::fmt;

impl fmt::Display for MixfixTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MixfixTree::Name(x, _) => write!(f, "{}", x),
            MixfixTree::Exp => write!(f, "_"),
            MixfixTree::Apply(e1, e2) => write!(f, "({} {})", *e1, *e2),
        }
    }
}

pub struct MixfixTreeVec<'a>(pub &'a Vec<MixfixTree>);

impl<'a> fmt::Display for MixfixTreeVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.len() {
            0 => Ok({}),
            1 => {
                write!(f, "{}", self.0[0])?;
                Ok({})
            },
            2 => {
                write!(f, "{} or {}", self.0[0], self.0[1])?;
                Ok({})
            }
            n => {
                for (i, t) in self.0.iter().enumerate() {
                    write!(f, "{}", t)?;
                    if i < n-2 {
                        write!(f, ", ")?;
                    }
                    else if i == n-2 {
                        write!(f, ", or ")?;
                    }
                }
                Ok({})
            }
        }
    }
}

// 1 + 2 + 3 -->
// ((`_ + _` ((`_ + _` 1) 2)) 3)
// the structure is
// Apply(
//   Apply(
//     Name(`_ + _`),
//     Apply(
//       Apply(
//         Name(`_ + _`),
//         Exp()),
//       Exp())),
// Exp())
// We can apply this tree to a list of expressions to create the
// correctly nested apply expression.

impl MixfixTree {
    pub fn make_call(e: MixfixTree, es: &[MixfixTree]) -> MixfixTree {
        if let Some((arg, args)) = es.split_first() {
            MixfixTree::make_call(
                MixfixTree::Apply(Box::new(e), Box::new(arg.clone())),
                args)
        }
        else {
            e
        }
    }

    pub fn make_nameless_call(args: &[MixfixTree]) -> Option<MixfixTree> {
        // If there were no name parts, just left associate all the expression into a call.
        if let Some((e, es)) = args.split_first() {
            Some(MixfixTree::make_call(e.clone(), es))
        }
        else {
            None
        }
    }

}
