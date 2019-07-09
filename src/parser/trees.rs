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
pub enum Kind {
    Type,
    Fun(Box<Kind>, Box<Kind>)
}

#[derive(Clone, Debug)]
pub enum Type {
    Path(Path),
    Part(Part),
    Var(Name),
    Fun(Box<Located<Type>>, Box<Located<Type>>),
    SelectMixfix(Path, Vec<Located<Type>>),
    Mixfix(Vec<Located<Type>>),
    Forall(Vec<Name>, Box<Located<Type>>),
    Tuple(Vec<Located<Type>>),
    List(Box<Located<Type>>),
    Kinded(Box<Located<Type>>, Located<Kind>),
    Dynamic,
}

#[derive(Clone, Debug)]
pub struct ModuleDef {
    pub attrs: Vec<Located<Attr>>,
    pub name: Name,
    pub defs: Vec<Located<Def>>,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub path: Located<Path>,
    pub selector: Vec<Located<Selector>>,
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
pub struct Name(pub Vec<Part>);

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
    pub attrs: Vec<Located<Attr>>,
    pub formula: Located<Formula>,
}

#[derive(Clone, Debug)]
pub struct LetDef {
    pub attrs: Vec<Located<Attr>>,
    pub formula: Located<Formula>,
}

#[derive(Clone, Debug)]
pub struct FunDef {
    pub attrs: Vec<Located<Attr>>,
    pub name: Name,
    pub opt_guard: Option<Located<Exp>>,
    pub opt_body: Option<Located<Exp>>,
    pub params: Vec<Located<Param>>,
    pub ret: Located<Param>,
    pub ret_type: Option<Located<Type>>,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub attrs: Vec<Located<Attr>>,
    pub name: Name,
    pub opt_guard: Option<Located<Exp>>,
    pub params: Vec<Located<Param>>,
    pub defs: Vec<Located<StructMember>>,  // only formula defs
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub attrs: Vec<Located<Attr>>,
    pub name: Name,
    pub params: Vec<Located<TypeParam>>,
    pub defs: Vec<Located<StructDef>>,  // only struct defs
}

#[derive(Clone, Debug)]
pub struct TraitDef {
    pub attrs: Vec<Located<Attr>>,
    pub name: Name,
    pub params: Vec<Located<TypeParam>>,
    pub supers: Vec<Located<Type>>,
    pub defs: Vec<Located<TraitMember>>,  
}

#[derive(Clone, Debug)]
pub enum TraitMember {
    Let(LetDef),
    Var(VarDef),
    Fun(FunDef),
    Import(Import),
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
    Import(Import),
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
    Colon,
    Comma,
    Dot,
    Eq,
    Hash,
    Semi,

    In,
    Out,
    Enum,
    Else,
    For,
    If,
    Cond,
    While,
    Struct,
    Match,
    Fun,
    Import,
    Let,
    Var,
    Trait,
    Where,
    Module,

    Lit { lit: Lit },
    Part { name: Part },
}

#[derive(Clone, Debug)]
pub enum Path {
    Root,
    Member(Box<Located<Path>>, Name), 
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
    Cond {
        cases: Vec<Located<Exp>>,
    },
    While {
        cond: Box<Located<Exp>>,
        body: Vec<Located<Exp>>,
    },
    DoWhile {
        body: Vec<Located<Exp>>,
        cond: Box<Located<Exp>>,
    },

    Block {
        body: Vec<Located<Exp>>,
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

    Ascribe {
        e: Box<Located<Exp>>,
        ty: Box<Located<Type>>,
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

