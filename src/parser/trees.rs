use num::bigint::BigInt;
use num::rational::BigRational;

use crate::syntax::names::Interned;
use crate::syntax::loc::*;

#[derive(Clone, Debug)]
pub enum Lit {
    Int { value: BigInt },
    Rat { value: BigRational },
    String { value: String },
    Char { value: char },
    Wildcard,
    Nothing,
}

#[derive(Clone, Debug)]
pub struct ParamAttr {
    pub attrs: Vec<Located<Attr>>,
    pub assoc: Assoc,
    pub by_name: CallingConv,
    pub mode: CallingMode,
}

#[derive(Clone, Debug)]
pub struct TypeParamAttr {
    pub attrs: Vec<Located<Attr>>,
    pub assoc: Assoc,
}

#[derive(Clone, Debug)]
pub enum CallingMode {
    Input,
    Output,
    Default,
}

#[derive(Clone, Debug)]
pub enum Assoc {
    NonAssoc,
    Assoc,
}

#[derive(Clone, Debug)]
pub enum CallingConv {
    ByValue,
    ByName,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub attr: ParamAttr,
    pub term: Box<Located<Exp>>,
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub attr: TypeParamAttr,
    pub ty: Box<Located<Type>>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Name(Name),
    Part(Part),
    Mixfix(Vec<Located<Type>>),
    Forall(Vec<Name>, Box<Located<Type>>),
    Dynamic,
}

#[derive(Clone, Debug)]
pub struct ModuleDef {
    imports: Vec<Located<Import>>,
    defs: Vec<Located<Def>>,
}

#[derive(Clone, Debug)]
pub struct Import {
    path: Located<Path>,
    selector: Vec<Located<Selector>>,
}

#[derive(Clone, Debug)]
pub enum Selector {
    All, 
    None,
    Including(Name),
    Excluding(Name),
    Renaming(Name, Name),
}

#[derive(Clone, Debug)]
pub enum Name {
    Id(Interned),
    Mixfix(Vec<Part>),
}

#[derive(Clone, Debug)]
pub enum Part {
    Id(Interned),
    Op(Interned),
    Placeholder,
}

// A pattern is an expression with unknowns.
// A formula is a boolean pattern.
pub type Formula = Pat;
pub type Pat = Exp;

#[derive(Clone, Debug)]
pub struct VarDef {
    attrs: Vec<Located<Attr>>,
    formula: Located<Formula>,
}

#[derive(Clone, Debug)]
pub struct LetDef {
    attrs: Vec<Located<Attr>>,
    formula: Located<Formula>,
}

#[derive(Clone, Debug)]
pub struct FunDef {
    attrs: Vec<Located<Attr>>,
    name: Name,
    opt_guard: Option<Located<Exp>>,
    opt_body: Option<Located<Exp>>,
    params: Vec<Located<Param>>,
    ret: Located<Param>,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    attrs: Vec<Located<Attr>>,
    name: Name,
    opt_guard: Option<Located<Exp>>,
    params: Vec<Located<Param>>,
    defs: Vec<Located<StructMember>>,  // only formula defs
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    attrs: Vec<Located<Attr>>,
    name: Name,
    params: Vec<Located<TypeParam>>,
    defs: Vec<Located<StructDef>>,  // only struct defs
}

#[derive(Clone, Debug)]
pub struct TraitDef {
    attrs: Vec<Located<Attr>>,
    name: Name,
    params: Vec<Located<TypeParam>>,
    supers: Vec<Located<Type>>,
    defs: Vec<Located<TraitMember>>,  
}

#[derive(Clone, Debug)]
pub enum TraitMember {
    Let(LetDef),
    Var(VarDef),
    Fun(FunDef)
}

#[derive(Clone, Debug)]
pub enum StructMember {
    Let(LetDef),
    Var(VarDef),
}

#[derive(Clone, Debug)]
pub enum Def {
    Error,
    Module(ModuleDef),
    Struct(StructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Fun(FunDef),
    Let(LetDef),
    Var(VarDef),
}

#[derive(Clone, Debug)]
pub enum BuiltinAttr {
    Prio(usize),
    Default,
    Unique,
    Open,
    Lazy,
    Type(Type),
}

// An attribute tree is basically just a token tree.
#[derive(Clone, Debug)]
pub enum Attr {
    Builtin(BuiltinAttr),

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

#[derive(Clone, Debug)]
pub enum Path {
    Root,
    Member(Box<Path>, Name), 
}

#[derive(Clone, Debug)]
pub enum Exp {
    Error,

    // These should really be syntactic sugar, but we bake them in
    // mainly to make AST building easier in the parser.
    Tuple {
        es: Vec<Located<Exp>>,
    },
    List {
        es: Vec<Located<Exp>>,
    },

    Lambda {
        params: Vec<Located<Pat>>,
        opt_guard: Option<Box<Located<Formula>>>,
        ret: Box<Located<Exp>>,
    },

    For {
        formula: Box<Located<Formula>>,
        body: Vec<Located<Exp>>,
        otherwise: Vec<Located<Exp>>,
    },
    Let {
        formula: Box<Located<Formula>>,
        body: Vec<Located<Exp>>,
        otherwise: Vec<Located<Exp>>,
    },
    LetVar {
        formula: Box<Located<Formula>>,
        body: Vec<Located<Exp>>,
        otherwise: Vec<Located<Exp>>,
    },
    Match {
        value: Box<Located<Exp>>,
        cases: Vec<Located<Exp>>,
    },

    Block {
        imports: Vec<Located<Import>>,
        defs: Vec<Located<Def>>,
        body: Box<Located<Exp>>,
    },

    Arrow {
        arg: Box<Located<Pat>>,
        ret: Box<Located<Exp>>,
    },
    Assign {
        lhs: Box<Located<Pat>>,
        rhs: Box<Located<Exp>>,
    },
    Bind {
        lhs: Box<Located<Pat>>,
        rhs: Box<Located<Exp>>,
    },
    Generator {
        lhs: Box<Located<Pat>>,
        rhs: Box<Located<Exp>>,
    },
    Where {
        pat: Box<Located<Pat>>,
        guard: Box<Located<Formula>>,
    },

    Select {
        module: Box<Located<Path>>,
        name: Name,
    },

    SelectMixfix {
        module: Box<Located<Path>>,
        es: Vec<Located<Exp>>,
    },

    Lit {
        lit: Lit,
    },

    // ambiguous names -- might resolve to variable names or parts of function names
    // or _ or ? or = as part of a function name or partial application
    Name {
        name: Name,
    },
    Var {
        name: Name,
    },
    Unknown {
        name: Name,
    },
    Part {
        part: Part,
    },

    Mixfix {
        es: Vec<Located<Exp>>,
    },

    Root,
}

#[derive(Clone, Debug)]
pub struct Root {
    pub defs: Vec<Located<Def>>,
}

