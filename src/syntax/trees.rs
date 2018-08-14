use num::bigint::BigInt;
use num::rational::BigRational;

use syntax::loc::Located;
use syntax::names::Name;
use syntax::symbols;


#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Lit {
    IntLit { value: BigInt },
    RatLit { value: BigRational },
    StringLit { value: String },
    CharLit { value: char },
    Wildcard,
    Nothing,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum FormulaFlag {
    Val,
    Var,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum MixfixFlag {
    Fun,
    Trait,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub assoc: bool,
    pub mode: CallingMode,
    pub pat: Box<Located<Exp>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum CallingMode {
    Input,
    Output,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Import {
    All { path: Box<Located<Exp>> },
    None { path: Box<Located<Exp>> },
    Including { path: Box<Located<Exp>>, name: Name },
    Excluding { path: Box<Located<Exp>>, name: Name },
    Renaming { path: Box<Located<Exp>>, name: Name, rename: Name },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Cmd {
    Def(Def),
    Exp(Exp),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    FormulaDef { flag: FormulaFlag, formula: Box<Located<Exp>> },
    MixfixDef { frame: symbols::Scope, flag: MixfixFlag, name: Name, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Param>>, ret: Located<Param> },
    ImportDef { import: Box<Located<Import>> },

    AmbMixfixDef { flag: MixfixFlag, name: Name, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Param>>, ret: Located<Param> },
    AmbImportDef { import: Box<Located<Import>> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Layout { body: Vec<Located<Cmd>>, frame: symbols::Scope },
    Trait { body: Vec<Located<Cmd>>, frame: symbols::Scope },

    CallByName { pat: Box<Located<Exp>> },

    Union { es: Vec<Located<Exp>> },
    Intersect { es: Vec<Located<Exp>> },

    Lambda { frame: symbols::Scope, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Exp>>, ret: Box<Located<Exp>> },
    For { frame: symbols::Scope, generator: Box<Located<Exp>>, body: Box<Located<Exp>> },

    Ascribe { exp: Box<Located<Exp>>, pat: Box<Located<Exp>> },
    Arrow { arg: Box<Located<Exp>>, ret: Box<Located<Exp>> },
    Assign { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Generator { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Bind { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },

    Select { exp: Box<Located<Exp>>, name: Name },
    Apply { fun: Box<Located<Exp>>, arg: Box<Located<Exp>> },

    Var { name: Name, decls: Vec<symbols::Decl> },
    Unknown { name: Name, decls: Vec<symbols::Decl> },
    MixfixPart { name: Name, decls: Vec<symbols::Decl> },

    Literal { lit: Lit },

    Native,
    GlobalFrame,
    CurrentFrame { scope: symbols::Scope },

    AmbLayout { body: Vec<Located<Cmd>> },
    AmbTrait { body: Vec<Located<Cmd>> },
    // Resolves to CurrentFrame or GlobalFrame.
    AmbFrame,
    AmbLambda { opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Exp>>, ret: Box<Located<Exp>> },
    AmbFor { generator: Box<Located<Exp>>, body: Box<Located<Exp>> },
    AmbWithin { e1: Box<Located<Exp>>, e2: Box<Located<Exp>> },

    // A union gets desugared into a trait with public imports (i.e., exports).
    // trait { fun x } with (A y)
    // ->
    // do { trait z = A y; trait { public import z._; fun x } }
    AmbUnion { es: Vec<Located<Exp>> },

    // ambiguous names -- might resolve to variable names or parts of function names
    // or _ or ? or = as part of a function name or partial application
    AmbName { name: Name },

    // Parsed trees
    MixfixApply { es: Vec<Located<Exp>> },

    // Prenamed trees
    VarRef { name: Name, lookup_ref: symbols::NameRef },
    MixfixRef { name: Name, lookup_ref: symbols::MixfixRef },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Root {
    Parsed { cmds: Vec<Located<Cmd>> },
    Prenamed { frame: symbols::Scope, cmds: Vec<Located<Cmd>> },
    Renamed { frame: symbols::Scope, cmds: Vec<Located<Cmd>> },
}
