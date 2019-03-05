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

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prio(pub usize);

impl std::iter::Step for Prio {
    fn add_one(&self) -> Prio {
        Prio(self.0+1)
    }
    fn sub_one(&self) -> Prio {
        Prio(self.0-1)
    }
    fn add_usize(&self, n: usize) -> Option<Prio> {
        Some(Prio(self.0+n))
    }
    fn steps_between(fst: &Prio, snd: &Prio) -> Option<usize> {
        if snd.0 >= fst.0 {
            Some(snd.0 - fst.0)
        }
        else {
            None
        }
    }
    fn replace_one(&mut self) -> Prio {
        self.0 = 1;
        *self
    }
    fn replace_zero(&mut self) -> Prio {
        self.0 = 0;
        *self
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Import {
    All { path: Ref },
    None { path: Ref },
    Here { path: Ref },
    Including { path: Ref, name: Name },
    Excluding { path: Ref, name: Name },
    Renaming { path: Ref, name: Name, rename: Name },
}

impl Import {
    pub fn path(&self) -> Ref {
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
    Select { outer: Box<StablePath>, name: Name },
    Apply { fun: Box<StablePath>, arg: Box<StablePath> },
}

// The symbol table is in graph.rs
// and consists of a set of Decl.
// Each Bundle has its own symbol table.
// Decls may have references to other decls. These may or may not be resolved.
// A resolved ref is just a GlobalRef. An unresolved ref requires a lookup/mixfix resolution operation be performed.
// Each Decl except (Decl::Bundle) has a parent Decl, always a LocalRef.
// Some Decls have members, always LocalDef.
// Some Decls have imports, with a unresolved Ref path.
// Some Decls have supers, a vec of unresolved Ref.
// The path of a Decl can be computed by following parent links.
// A GlobalRef refers to a particular Decl, possibly in another Bundle.
// A LocalRef refers to a particular Decl in the same Bundle.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalRef {
    pub bundle: crate::driver::BundleIndex,
    pub local_ref: LocalRef,
}

pub type LocalRef = EnvIndex;

impl LocalRef {
    pub fn to_global_ref(self, bundle: crate::driver::BundleIndex) -> GlobalRef {
        GlobalRef { bundle, local_ref: self }
    }
    pub fn to_ref(self, bundle: crate::driver::BundleIndex) -> Ref {
        self.to_global_ref(bundle).to_ref()
    }
}
impl GlobalRef {
    pub fn to_ref(self) -> Ref {
        Ref::Resolved(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ref {
    Root,
    Resolved(GlobalRef),
    Lookup(LookupIndex),
    Mixfix(MixfixIndex),
}

trait Resolver {
    fn resolve(&self) -> Vec<GlobalRef>;
}

impl Resolver for Ref {
    fn resolve(&self) -> Vec<GlobalRef> {
        match self {
            Ref::Resolved(r) => vec![*r],
            Ref::Lookup(idx) => unimplemented!(),
            Ref::Mixfix(idx) => unimplemented!(),
            Ref::Root => unimplemented!(),
        }
    }
}

// A lookup of a name traverses from the current scope up and out (and possibly in for imports).
// We return the set of decls in scope with that name.
// To get the path of a decl, we just follow parent links.

// Treat a declaration as an Env.
pub trait DeclEnv {
    fn parent(&self) -> Option<LocalRef>;
    fn imports(&self) -> Vec<Located<Import>>;
    fn supers(&self) -> Vec<Ref>;
    fn lookup_member(&self, name: Name) -> Vec<LocalRef>;
    fn path(&self, graph: &crate::namer::graph::ScopeGraph) -> StablePath;
}

impl DeclEnv for Decl {
    fn parent(&self) -> Option<LocalRef> {
        match self {
            Decl::Root => None,
            Decl::Bundle { .. } => None, // should be the index of Root, but Root has no index.
            Decl::Block { parent, .. } => Some(*parent), // should be the index of Root, but Root has no index.
            Decl::Trait { parent, .. } => Some(*parent),
            Decl::Fun { parent, .. } => Some(*parent),
            Decl::Val { parent, .. } => Some(*parent),
            Decl::Var { parent, .. } => Some(*parent),
            // Decl::Val { scope: Scope::Env(index), .. } => *index,
            // Decl::Var { scope: Scope::Env(index), .. } => *index,
            _ => unimplemented!(),
        }
    }

    fn imports(&self) -> Vec<Located<Import>> { unimplemented!() }

    fn supers(&self) -> Vec<Ref> {
        match self {
            Decl::Trait { supers, .. } => supers.clone(),
            _ => vec![],
        }
    }

// FIXME: some Decls can be used as Scope and others no.
// Root should not be a Decl. Indeed we should distinguish them again. Che shit.
    fn path(&self, graph: &crate::namer::graph::ScopeGraph) -> StablePath {
        match self {
            Decl::Root => StablePath::Root,
            Decl::Bundle { .. } => StablePath::Root,
            Decl::Trait { parent: parent_index, name, .. } => {
                let parent = graph.get_env(*parent_index);
                StablePath::Select {
                    outer: box parent.path(graph),
                    name: *name
                }
            },
            Decl::Block { parent: parent_index, .. } => {
                let parent = graph.get_env(*parent_index);
                parent.path(graph)
            },
            Decl::Fun { parent: parent_index, name, .. } => {
                let parent = graph.get_env(*parent_index);
                StablePath::Select {
                    outer: box parent.path(graph),
                    name: *name
                }
            }
            Decl::Val { parent: parent_index, name } => {
                let parent = graph.get_env(*parent_index);
                StablePath::Select {
                    outer: box parent.path(graph),
                    name: *name
                }
            },
            Decl::Var { parent: parent_index, name } => {
                let parent = graph.get_env(*parent_index);
                StablePath::Select {
                    outer: box parent.path(graph),
                    name: *name
                }
            },
            Decl::MixfixPart { .. } => StablePath::Unstable, // unreachable!(),
        }
    }

    fn lookup_member(&self, name: Name) -> Vec<LocalRef> {
        match self {
            Decl::Bundle { members, .. } => {
                match members.get(&name) {
                    Some(lrefs) => lrefs.clone(),
                    None => vec![],
                }
            },
            Decl::Block { members, .. } => {
                match members.get(&name) {
                    Some(lrefs) => lrefs.clone(),
                    None => vec![],
                }
            },
            Decl::Trait { members, .. } => {
                match members.get(&name) {
                    Some(lrefs) => lrefs.clone(),
                    None => vec![],
                }
            },
            _ => vec![],
        }
    }
}

impl Decl {
    pub fn new_bundle(index: crate::driver::BundleIndex) -> Decl {
        Decl::Bundle {
            index: index,
            imports: vec![],
            members: BTreeMap::new(),
        }
    }
    pub fn new_trait(parent: LocalRef, name: Name, prio: Prio, params: Vec<ParamAttr>) -> Decl {
        Decl::Trait {
            parent: parent,
            name: name,
            prio: prio,
            params: params,
            supers: vec![],
            imports: vec![],
            members: BTreeMap::new(),
        }
    }
    pub fn new_fun(parent: LocalRef, name: Name, prio: Prio, params: Vec<ParamAttr>, ret: ParamAttr) -> Decl {
        Decl::Fun {
            parent: parent,
            name: name,
            prio: prio,
            params: params,
            ret: ret,
        }
    }
    pub fn new_block(parent: LocalRef) -> Decl {
        Decl::Block {
            parent: parent,
            imports: vec![],
            members: BTreeMap::new(),
        }
    }
    pub fn new_val(parent: LocalRef, name: Name) -> Decl {
        Decl::Val {
            parent: parent,
            name: name
        }
    }
    fn new_var(parent: LocalRef, name: Name) -> Decl {
        Decl::Var {
            parent: parent,
            name: name
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Decl {
    Root, // This is just used for imports.

    // Members are anything but Bundle or Root.
    // Because of overloading, a name can map to multiple definitions (even of different kinds).
    Bundle {
        index: crate::driver::BundleIndex,
        imports: Vec<Located<Import>>,
        members: BTreeMap<Name, Vec<LocalRef>>,
    },

    Trait {
        parent: LocalRef,
        name: Name,
        prio: Prio,
        params: Vec<ParamAttr>,
        supers: Vec<Ref>,
        imports: Vec<Located<Import>>,
        members: BTreeMap<Name, Vec<LocalRef>>,
    },

    // Represents the body of a function.
    Block {
        parent: LocalRef,
        imports: Vec<Located<Import>>,
        members: BTreeMap<Name, Vec<LocalRef>>,
    },

    Fun {
        parent: LocalRef,
        name: Name,
        prio: Prio,
        params: Vec<ParamAttr>,
        ret: ParamAttr,
    },

    Val {
        parent: LocalRef,
        name: Name,
    },

    Var {
        parent: LocalRef,
        name: Name,
    },

    MixfixPart {
        name: Name,
        index: usize,
        full: Name,
        orig: LocalRef,
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
            _ => unimplemented!(),
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

    pub fn prio(&self) -> Prio {
        match self {
            Decl::Trait { prio, .. } => *prio,
            Decl::Fun { prio, .. } => *prio,
            _ => Prio(0),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LookupRef {
    // Lookup name starting in scope and following supers, imports, and parents.
    From { scope: LocalRef, name: Name, follow_imports: bool },
    // Lookup name as a member of the given scope or its supers, but not its imports or parents.
    Within { scope: Ref, name: Name },
}

impl LookupRef {
    pub fn new(scope: LocalRef, name: Name, follow_imports: bool) -> LookupRef {
        LookupRef::From {
            scope,
            name,
            follow_imports
        }
    }

    pub fn as_member(scope: Ref, name: Name) -> LookupRef {
        LookupRef::Within {
            scope,
            name
        }
    }

    pub fn name(&self) -> Name {
        match self {
            LookupRef::From { name, .. } => *name,
            LookupRef::Within { name, .. } => *name,
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MixfixTree {
    Name(Name, Vec<GlobalRef>), // store the grefs and the decls in the name to avoid lookups. FIXME: borrow the decl to avoid cloning.
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
                MixfixTree::Apply(box e, box arg.clone()),
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
