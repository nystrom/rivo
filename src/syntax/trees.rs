use num::bigint::BigInt;
use num::rational::BigRational;

use syntax::loc::Located;
use syntax::names::Name;

pub mod symbols {
    // FIXME
    #[derive(Clone, Debug)]
    pub struct Scope;

    #[derive(Clone, Debug)]
    pub struct Decl;

    #[derive(Clone, Debug)]
    pub struct NameRef;

    #[derive(Clone, Debug)]
    pub struct MixfixRef;
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Lit {
    IntLit { value: BigInt },
    RatLit { value: BigRational },
    StringLit { value: String },
    CharLit { value: char },
    Wildcard,
    Nothing,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum FormulaFlag {
    Val,
    Var,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum MixfixFlag {
    Fun,
    Trait,
}

#[derive(Clone, Debug)]
pub struct Param {
    assoc: bool,
    mode: CallingMode,
    pat: Box<Located<Tree>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum CallingMode {
    Input,
    Output,
}

#[derive(Clone, Debug)]
pub enum Import {
    All { path: Box<Located<Tree>> },
    None { path: Box<Located<Tree>> },
    Including { path: Box<Located<Tree>>, name: Name },
    Excluding { path: Box<Located<Tree>>, name: Name },
    Renaming { path: Box<Located<Tree>>, name: Name, rename: Name },
}

#[derive(Clone, Debug)]
pub enum Tree {
    Bundle { cmds: Vec<Located<Tree>>, frame: symbols::Scope },
    Layout { body: Vec<Located<Tree>>, frame: symbols::Scope },
    Trait { body: Vec<Located<Tree>>, frame: symbols::Scope },

    FormulaDef { flag: FormulaFlag, formula: Box<Located<Tree>> },
    MixfixDef { frame: symbols::Scope, flag: MixfixFlag, name: Name, opt_guard: Option<Box<Located<Tree>>>, params: Vec<Param>, ret: Param },
    ImportDef { import: Box<Import> },

    CallByName { pat: Box<Located<Tree>> },

    Union { es: Vec<Located<Tree>> },
    Intersect { es: Vec<Located<Tree>> },

    Lambda { frame: symbols::Scope, opt_guard: Option<Box<Located<Tree>>>, params: Vec<Located<Tree>>, ret: Box<Located<Tree>> },
    For { frame: symbols::Scope, generator: Box<Located<Tree>>, body: Box<Located<Tree>> },

    Ascribe { exp: Box<Located<Tree>>, pat: Box<Located<Tree>> },
    Arrow { arg: Box<Located<Tree>>, ret: Box<Located<Tree>> },
    Assign { lhs: Box<Located<Tree>>, rhs: Box<Located<Tree>> },
    Generator { lhs: Box<Located<Tree>>, rhs: Box<Located<Tree>> },
    Bind { lhs: Box<Located<Tree>>, rhs: Box<Located<Tree>> },

    Select { exp: Box<Located<Tree>>, name: Name },
    Apply { fun: Box<Located<Tree>>, arg: Box<Located<Tree>> },

    Var { name: Name, decls: Vec<symbols::Decl> },
    Unknown { name: Name, decls: Vec<symbols::Decl> },
    MixfixPart { name: Name, decls: Vec<symbols::Decl> },

    Literal { lit: Lit },

    Native,
    GlobalFrame,
    CurrentFrame { scope: symbols::Scope },

    // Parsed trees
    AmbBundle { cmds: Vec<Located<Tree>>, frame: symbols::Scope },
    AmbLayout { body: Vec<Located<Tree>>, frame: symbols::Scope },
    AmbTrait { body: Vec<Located<Tree>>, frame: symbols::Scope },
    // Resolves to CurrentFrame or GlobalFrame.
    AmbFrame,
    AmbLambda { opt_guard: Option<Box<Located<Tree>>>, params: Vec<Located<Tree>>, ret: Box<Located<Tree>> },
    AmbFor { generator: Box<Located<Tree>>, body: Box<Located<Tree>> },
    AmbWithin { e1: Box<Located<Tree>>, e2: Box<Located<Tree>> },
    AmbMixfixDef { flag: MixfixFlag, name: Name, opt_guard: Option<Box<Located<Tree>>>, params: Vec<Param>, ret: Param },

    // A union gets desugared into a trait with public imports (i.e., exports).
    // trait { fun x } with (A y)
    // ->
    // do { trait z = A y; trait { public import z._; fun x } }
    AmbUnion { es: Vec<Located<Tree>> },
    AmbImportDef { import: Box<Import> },
    // ambiguous names -- might resolve to variable names or parts of function names
    // or _ or ? or = as part of a function name or partial application
    AmbName { name: Name },

    // Prenamed trees
    MixfixApply { es: Vec<Located<Tree>> },
    VarRef { name: Name, lookup_ref: symbols::NameRef },
    MixfixRef { name: Name, lookup_ref: symbols::MixfixRef },
}
