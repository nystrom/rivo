use num::bigint::BigInt;
use num::rational::BigRational;
use std::fmt;

use syntax::loc::Located;
use syntax::names::Name;
use namer::graph::LookupIndex;
use namer::graph::MixfixIndex;

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
pub enum Def {
    FormulaDef { attrs: Vec<Located<Attr>>, flag: FormulaFlag, formula: Box<Located<Exp>> },
    MixfixDef { id: NodeId, attrs: Vec<Located<Attr>>, flag: MixfixFlag, name: Name, opt_guard: Option<Box<Located<Exp>>>, opt_body: Option<Box<Located<Exp>>>, params: Vec<Located<Param>>, ret: Located<Param> },
    ImportDef { opt_path: Option<Box<Located<Exp>>>, selector: Selector },
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
    BackArrow,
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
    Val,
    Var,
    Trait,
    With,
    Where,

    Lit { lit: Lit },
    Name { name: Name },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Layout { id: NodeId, cmds: Vec<Located<Cmd>> },

    Record { id: NodeId, tag: Box<Located<Exp>>, defs: Vec<Located<Def>> },
    // Tag of the enclosing trait.
    Outer,

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
