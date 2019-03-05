use num::bigint::BigInt;
use num::rational::BigRational;
use std::fmt;

use syntax::loc::Located;
use syntax::names::Name;
use namer::graph::LookupIndex;
use namer::graph::MixfixIndex;




// FIXME: create two different syntaxes (HACK? use include!).
// Or parameterize on the scopes somehow?

// We use node ids to index into the naming data structures.
// Only nodes that introduce interact with naming have identifiers.
// We embed name, mixfix, and scope ids in the AST.
// These are used by the (Pre)namer.
// The AST is not modified by the prenamer, but is rewritten
// by the namer, replacing MixfixApply with Apply.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "node {}", self.0)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeIdGenerator {
    next: usize
}

impl NodeIdGenerator {
    pub fn new() -> NodeIdGenerator {
        NodeIdGenerator { next: 0 }
    }

    pub fn new_id(&mut self) -> NodeId {
        let index = self.next;
        self.next += 1;
        NodeId(index)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Lit {
    Int { value: BigInt },
    Rat { value: BigRational },
    String { value: String },
    Char { value: char },
    Wildcard,
    Nothing,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum FormulaFlag {
    Val,
    Var,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum MixfixFlag {
    Fun,
    Trait,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParamAttr {
    pub assoc: Assoc,
    pub by_name: CallingConv,
    pub mode: CallingMode,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum CallingMode {
    Input,
    Output,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Assoc {
    NonAssoc,
    Assoc,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum CallingConv {
    ByValue,
    ByName,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub attr: ParamAttr,
    pub pat: Box<Located<Exp>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Cmd {
    Def(Def),
    Exp(Exp),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MixfixHeader {
    attrs: Vec<Located<Attr>>,
    name: Name,
    opt_guard: Option<Box<Located<Exp>>>,
    params: Vec<Located<Param>>
}

#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    ImportDef { opt_path: Option<Box<Located<Exp>>>, selector: Selector },
    FormulaDef { attrs: Vec<Located<Attr>>, flag: FormulaFlag, formula: Box<Located<Exp>> },
    FunDef { id: NodeId, attrs: Vec<Located<Attr>>, name: Name, opt_guard: Option<Box<Located<Exp>>>, opt_body: Option<Box<Located<Exp>>>, params: Vec<Located<Param>>, ret: Located<Param> },
    TraitDef { id: NodeId, attrs: Vec<Located<Attr>>, name: Name, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Param>>, supers: Vec<Located<Exp>>, defs: Vec<Located<Def>> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Selector {
    All,
    Nothing,
    Including { name: Name },
    Excluding { name: Name },
    Renaming { name: Name, rename: Name },
}

// An attribute tree is basically just a token tree.
#[derive(Clone, Debug, PartialEq)]
pub enum Attr {
    // Sequences of token trees.
    Brackets(Box<Located<Attr>>),
    Braces(Box<Located<Attr>>),
    Parens(Box<Located<Attr>>),
    CommaSeq(Vec<Located<Attr>>),
    Seq(Vec<Located<Attr>>),

    Arrow,
    Assign,
    At,
    Backarrow,
    Bang,
    Colon,
    Comma,
    Dot,
    Eq,
    Hash,
    Question,
    Semi,

    For,
    Fun,
    Import,
    Let,
    Val,
    Var,
    Trait,
    Where,
    With,

    Lit { lit: Lit },
    Name { name: Name },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    name: Name,
    value: Located<Exp>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Layout { id: NodeId, cmds: Vec<Located<Cmd>> },

    // A with B
    Union { e1: Box<Located<Exp>>, e2: Box<Located<Exp>> },
    // A @ B
    Intersect { e1: Box<Located<Exp>>, e2: Box<Located<Exp>> },

    // These are used by the cheap renamer.
    OrElse { e1: Box<Located<Exp>>, e2: Box<Located<Exp>> },
    TrySelect { exp: Box<Located<Exp>>, name: Name },
    AnyOf { es: Vec<Located<Exp>> },
    Fail { message: String },
    Global,

    // These should really be syntactic sugar, but we bake them in
    // mainly to make AST building easier in the parser.
    Tuple { es: Vec<Located<Exp>> },
    List { es: Vec<Located<Exp>> },

    Lambda { id: NodeId, opt_guard: Option<Box<Located<Exp>>>, params: Vec<Located<Exp>>, ret: Box<Located<Exp>> },
    For { id: NodeId, formula: Box<Located<Exp>>, body: Box<Located<Exp>> },
    Let { id: NodeId, formula: Box<Located<Exp>>, body: Box<Located<Exp>> },
    LetVar { id: NodeId, formula: Box<Located<Exp>>, body: Box<Located<Exp>> },

    Arrow { id: NodeId, arg: Box<Located<Exp>>, ret: Box<Located<Exp>> },
    Assign { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Bind { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Generator { lhs: Box<Located<Exp>>, rhs: Box<Located<Exp>> },
    Where { pat: Box<Located<Exp>>, guard: Box<Located<Exp>> },

    Select { exp: Box<Located<Exp>>, name: Name },

    // e1.e2 is sugar for { import e1._, e2 }
    Within { id: NodeId, e1: Box<Located<Exp>>, e2: Box<Located<Exp>> },

    Apply { fun: Box<Located<Exp>>, arg: Box<Located<Exp>> },

    Lit { lit: Lit },

    // ambiguous names -- might resolve to variable names or parts of function names
    // or _ or ? or = as part of a function name or partial application
    Name { name: Name, id: NodeId },

    Unknown { name: Name, id: NodeId, },
    MixfixPart { name: Name, id: NodeId, },
    Var { name: Name, id: NodeId, },

    // Parsed trees
    MixfixApply { es: Vec<Located<Exp>>, id: NodeId },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Root {
    Bundle { id: NodeId, cmds: Vec<Located<Cmd>> },
}
