use num::bigint::BigInt;
use num::rational::BigRational;

use syntax::loc::Located;
use syntax::names::Name;

// FIXME: create two different syntaxes (HACK? use include!).
// Or parameterize on the scopes somehow?
// add a id to nodes that create a new scope.
// don't use String, but use &'a str where 'a is the lifetime of the
// ast.

// We use node ids to index into the naming data structures.
// Only nodes that introduce interact with naming have identifiers.
// We embed name, mixfix, and scope ids in the AST.
// These are used by the (Pre)namer.
// The AST is not modified by the prenamer, but is rewritten
// by the namer, replacing MixfixApply with Apply.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeId(pub usize);

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
    MixfixDef { id: NodeId, flag: MixfixFlag, name: Name, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Param>>, ret: Located<Param> },
    ImportDef { import: Box<Located<Exp>> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Layout { id: NodeId, cmds: Vec<Located<Cmd>> },
    Record { id: NodeId, defs: Vec<Located<Def>> },

    // A with B
    Union { es: Vec<Located<Exp>> },
    // A @ B
    Intersect { es: Vec<Located<Exp>> },

    // These should really be syntactic sugar, but we bake them in
    // mainly to make AST building easier in the parser.
    Tuple { es: Vec<Located<Exp>> },
    List { es: Vec<Located<Exp>> },

    Lambda { id: NodeId, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Exp>>, ret: Box<Located<Exp>> },
    For { id: NodeId, generator: Box<Located<Exp>>, body: Box<Located<Exp>> },

    Ascribe { exp: Box<Located<Exp>>, pat: Box<Located<Exp>> },
    Arrow { id: NodeId, arg: Box<Located<Exp>>, ret: Box<Located<Exp>> },
    Assign { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Generator { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Bind { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },

    Select { exp: Box<Located<Exp>>, name: Name },

    // e1.e2 is sugar for { import e1._, e2 }
    Within { id: NodeId, e1: Box<Located<Exp>>, e2: Box<Located<Exp>> },

    Apply { fun: Box<Located<Exp>>, arg: Box<Located<Exp>> },

    Lit { lit: Lit },

    Native,

    // Resolves to CurrentFrame or GlobalFrame.
    Frame { id: NodeId },

    // ambiguous names -- might resolve to variable names or parts of function names
    // or _ or ? or = as part of a function name or partial application
    Name { name: Name, id: NodeId },

    // Parsed trees
    MixfixApply { es: Vec<Located<Exp>>, id: NodeId },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Root {
    Bundle { id: NodeId, cmds: Vec<Located<Cmd>> },
}
