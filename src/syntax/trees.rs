use num::bigint::BigInt;
use num::rational::BigRational;

use syntax::loc::Located;
use syntax::names::Name;

// We embed name, mixfix, and scope ids in the AST.
// These are used by the (Pre)namer.
// The AST is not modified by the prenamer, but is rewritten
// by the namer, replacing MixfixApply with Apply.
pub type NameId = u32;
pub type MixfixId = u32;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ScopeId {
    Empty,
    Global,
    Scope(u32)
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Lit {
    Int { value: BigInt },
    Rat { value: BigRational },
    String { value: String },
    Char { value: char },
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
    pub assoc: Assoc,
    pub by_name: CallingConv,
    pub mode: CallingMode,
    pub pat: Box<Located<Exp>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum CallingMode {
    Input,
    Output,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Assoc {
    NonAssoc,
    Assoc,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum CallingConv {
    ByValue,
    ByName,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Cmd {
    Def(Def),
    Exp(Exp),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    FormulaDef { flag: FormulaFlag, formula: Box<Located<Exp>> },
    MixfixDef { scope_id: ScopeId, flag: MixfixFlag, name: Name, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Param>>, ret: Located<Param> },
    ImportDef { import: Box<Located<Exp>> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Layout { scope_id: ScopeId , cmds: Vec<Located<Cmd>> },
    Trait { scope_id: ScopeId, defs: Vec<Located<Def>> },

    // A with B
    Union { es: Vec<Located<Exp>> },
    // A @ B
    Intersect { es: Vec<Located<Exp>> },

    // These should really be syntactic sugar, but we bake them in
    // mainly to make AST building easier in the parser.
    Tuple { es: Vec<Located<Exp>> },
    List { es: Vec<Located<Exp>> },

    Lambda { scope_id: ScopeId, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Exp>>, ret: Box<Located<Exp>> },
    For { scope_id: ScopeId, generator: Box<Located<Exp>>, body: Box<Located<Exp>> },

    Ascribe { exp: Box<Located<Exp>>, pat: Box<Located<Exp>> },
    Arrow { arg: Box<Located<Exp>>, ret: Box<Located<Exp>> },
    Assign { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Generator { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Bind { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },

    Select { exp: Box<Located<Exp>>, name: Name },

    // e1.e2 is sugar for { import e1._, e2 }
    Within { scope_id: ScopeId, e1: Box<Located<Exp>>, e2: Box<Located<Exp>> },

    Apply { fun: Box<Located<Exp>>, arg: Box<Located<Exp>> },

    Lit { lit: Lit },

    Native,

    // Resolves to CurrentFrame or GlobalFrame.
    Frame { scope_id: ScopeId },

    // ambiguous names -- might resolve to variable names or parts of function names
    // or _ or ? or = as part of a function name or partial application
    Name { name: Name, id: NameId },

    // Parsed trees
    MixfixApply { es: Vec<Located<Exp>>, id: MixfixId },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Root {
    Bundle { scope_id: ScopeId, cmds: Vec<Located<Cmd>> },
}
