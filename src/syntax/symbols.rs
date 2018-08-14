use syntax::names::Name;
use syntax::trees::CallingMode;

#[derive(Clone, Debug, PartialEq)]
pub enum Scope {
    EmptyScope,
    GlobalScope,
    RefScope(Ref),
    EnvScope(Env),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    pub decls: Vec<Decl>,
    pub imports: Vec<Import>,
    pub parents: Vec<Scope>,
    pub includes: Vec<Scope>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Import {
    All { path: Box<Scope> },
    None { path: Box<Scope> },
    Including { path: Box<Scope>, name: Name },
    Excluding { path: Box<Scope>, name: Name },
    Renaming { path: Box<Scope>, name: Name, rename: Name },
}

impl Decl {
    fn name(&self) -> Name {
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
pub enum Decl {
    Trait {
        scope: Box<Scope>,
        name: Name,
        assoc: Vec<bool>,
        param_modes: Vec<CallingMode>,
        ret_mode: CallingMode,
        prio: u32,
        body: Vec<Scope>,
    },

    Fun {
        scope: Box<Scope>,
        name: Name,
        assoc: Vec<bool>,
        param_modes: Vec<CallingMode>,
        ret_mode: CallingMode,
        prio: u32,
    },

    Val {
        scope: Box<Scope>,
        name: Name,
    },

    Var {
        scope: Box<Scope>,
        name: Name,
    },

    MixfixPart {
        name: Name,
        index: u32,
        full: Name,
        orig: Box<Decl>
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ref {
    Name(NameRef),
    Mixfix(MixfixRef),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NameRef {
    Lookup(LookupRef),
    LookupHere(LookupHereRef),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LookupRef {
    pub scope: Box<Scope>,
    pub name: Name,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LookupHereRef {
    pub scope: Box<Scope>,
    pub name: Name,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MixfixRef {
    pub scope: Box<Scope>,
    pub parts: Vec<Part>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Part {
    Name(NameRef),
    Placeholder,
}

pub enum MixfixTree {
    Name(Name, Vec<Decl>),
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
