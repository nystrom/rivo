use syntax::names::Name;

#[derive(Clone, Debug)]
pub enum Scope {
    RefScope(Ref),
    EnvScope(Env),
}

#[derive(Clone, Debug)]
pub struct Env {
    pub decls: Vec<Decl>,
    pub imports: Vec<Import>,
    pub parents: Vec<Scope>,
    pub includes: Vec<Scope>,
}

#[derive(Clone, Debug)]
pub enum Import {
    All { path: Box<Scope> },
    None { path: Box<Scope> },
    Including { path: Box<Scope>, name: Name },
    Excluding { path: Box<Scope>, name: Name },
    Renaming { path: Box<Scope>, name: Name, rename: Name },
}

#[derive(Clone, Debug)]
pub enum Decl {
    Trait(TraitDecl),
    Fun(FunDecl),
    Val(ValDecl),
    Var(VarDecl),
}

#[derive(Clone, Debug)]
pub struct TraitDecl;
#[derive(Clone, Debug)]
pub struct FunDecl;
#[derive(Clone, Debug)]
pub struct ValDecl;
#[derive(Clone, Debug)]
pub struct VarDecl;

#[derive(Clone, Debug)]
pub enum Ref {
    Name(NameRef),
    Mixfix(MixfixRef),
}

#[derive(Clone, Debug)]
pub enum NameRef {
    Lookup(LookupRef),
    LookupHere(LookupHereRef),
}

#[derive(Clone, Debug)]
pub struct LookupRef {
    pub scope: Box<Scope>,
    pub name: Name,
}

#[derive(Clone, Debug)]
pub struct LookupHereRef {
    pub scope: Box<Scope>,
    pub name: Name,
}

#[derive(Clone, Debug)]
pub struct MixfixRef {
    pub scope: Box<Scope>,
    pub parts: Vec<Part>,
}

#[derive(Clone, Debug)]
pub enum Part {
    Name(NameRef),
    Placeholder,
}
