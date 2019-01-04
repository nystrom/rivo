use syntax::trees::*;
use syntax::names::*;
use syntax::loc::*;
use pretty::*;

#[macro_export]
macro_rules! show_located {
    ($located: expr) => {
        $located.value.to_doc()
    };
}

#[macro_export]
macro_rules! show_located_exp_vec {
    ($list: expr, $sep: expr) => {
        Doc::intersperse(
            $list.into_iter().map(
                |located| {
                    match located {
                        Located { loc, value: Exp::Name { .. } } => show_located!(located),
                        Located { loc, value: Exp::Lit { .. } } => show_located!(located),
                        _ => Doc::text("(")
                                .append(show_located!(located))
                                .append(Doc::text(")")),
                    }
                }
            ),
            $sep
        ).group()
    };
}
#[macro_export]
macro_rules! show_located_vec {
    ($list: expr, $sep: expr) => {
        Doc::intersperse(
            $list.into_iter().map(
                |located| show_located!(located)
            ),
            $sep
        ).group()
    };
}

#[macro_export]
macro_rules! show_located_box {
    ($located: expr) => {
        show_located!(*$located)
    };
}

#[macro_export]
macro_rules! show_located_opt {
    ($located: expr) => {
        match $located {
            Some(e) => Doc::space().append(show_located_box!(e)),
            None => Doc::space()
        }
    };
}

pub trait ToDoc {
    fn to_doc(&self) -> Doc<BoxDoc<()>>;

    fn pretty(&self, width: usize) -> String {
        format!("{}", self.to_doc().pretty(width))
    }
}

impl ToDoc for Name {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Name::Id(x) => Doc::text(x.to_string()),
            Name::Op(x) => Doc::text(x.to_string()),
            Name::Mixfix(s) => Doc::text("`").append(Doc::text(s.to_string())).append(Doc::text("`")),
            //     let parts = Name::decode_parts(*s);
            //     let docs: Vec<Doc<BoxDoc<()>>> = parts.iter().cloned().map(|part| part.to_doc()).collect();
            //     Doc::text("`")
            //     .append(
            //         Doc::intersperse(docs, Doc::space())
            //     )
            //     .append(Doc::text("`"))
            // },
        }
    }
}

impl ToDoc for Part {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Part::Id(ref x) => Doc::text(x.to_string()),
            Part::Op(ref x) => Doc::text(x.to_string()),
            Part::Placeholder => Doc::text("_"),
        }
    }
}

impl ToDoc for Lit {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Lit::Int { ref value } => Doc::text(format!("{}", value)),
            Lit::Rat { ref value } => Doc::text(format!("{}", value)),
            Lit::String { ref value } => Doc::text(format!("\"{}\"", value)),
            Lit::Char { ref value } => Doc::text(format!("'{}'", value)),
            Lit::Wildcard => Doc::text("_"),
            Lit::Nothing => Doc::text("()"),
        }
    }
}

impl ToDoc for Root {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Root::Bundle { id, ref cmds } =>
                show_located_vec!(cmds, Doc::text(";").append(Doc::newline())),
        }
    }
}

impl ToDoc for Cmd {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Cmd::Def(ref d) => d.to_doc(),
            Cmd::Exp(ref e) => e.to_doc(),
        }
    }
}

impl ToDoc for Def {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Def::FormulaDef { ref attrs, ref flag, ref formula } => {
                let doc = match *flag {
                    FormulaFlag::Val => Doc::text("val"),
                    FormulaFlag::Var => Doc::text("var"),
                };
                doc.append(Doc::space())
                   .append(show_located_box!(formula))
            },
            Def::MixfixDef { id, ref attrs, ref flag, ref name, ref opt_guard, ref opt_body, ref params, ref ret } => {
                let doc = match *flag {
                    MixfixFlag::Trait => Doc::text("trait"),
                    MixfixFlag::Fun => Doc::text("fun"),
                };
                doc.append(Doc::space())
                   .append(name.to_doc())
                   .append(Doc::space())
                   .append(show_located_vec!(params, Doc::space()))
                   .append(show_located_opt!(opt_guard))
                   .append(Doc::space())
                   .append(Doc::text("="))
                   .append(Doc::space())
                   .append(show_located!(ret))
                   .append(show_located_opt!(opt_body))
            },
            Def::ImportDef { opt_path: None, ref selector } => {
                Doc::text("import")
                   .append(Doc::space())
                   .append(selector.to_doc())
            },
            Def::ImportDef { opt_path: Some(ref path), ref selector } => {
                Doc::text("import")
                   .append(Doc::space())
                   .append(show_located_box!(path))
                   .append(Doc::text("."))
                   .append(selector.to_doc())
            },
        }
    }
}

impl ToDoc for Selector {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Selector::Nothing => Doc::text("()"),
            Selector::All => Doc::text("_"),
            Selector::Including { name } => name.to_doc(),
            Selector::Excluding { name } => name.to_doc().append(Doc::space()).append("->").append(Doc::space()).append(Doc::text("()")),
            Selector::Renaming { name, rename } => name.to_doc().append(Doc::space()).append("->").append(Doc::space()).append(rename.to_doc()),
        }
    }
}

impl ToDoc for Exp {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Exp::Layout { id, ref cmds } =>
                Doc::text("{")
                .append(Doc::newline())
                .append(show_located_vec!(cmds, Doc::text(";").append(Doc::newline())).nest(1))
                .append(Doc::newline())
                .append(Doc::text("}")),
            Exp::Record { id, ref tag, ref defs } =>
                show_located_box!(tag)
                .append(Doc::space())
                .append(Doc::text("{"))
                .append(Doc::newline())
                .append(show_located_vec!(defs, Doc::text(";").append(Doc::newline())).nest(1))
                .append(Doc::newline())
                .append(Doc::text("}")),
            Exp::Outer { .. } => Doc::text("outer"),

            Exp::Union { ref es } =>
                show_located_vec!(es, Doc::space().append(Doc::text("with")).append(Doc::space())),
            Exp::Intersect { ref es } =>
                show_located_vec!(es, Doc::space().append(Doc::text("@")).append(Doc::space())),

            Exp::Tuple { ref es } =>
                Doc::text("(")
                .append(show_located_vec!(es, Doc::text(", ")))
                .append(Doc::text(")")),
            Exp::List { ref es } =>
                Doc::text("[")
                .append(show_located_vec!(es, Doc::text(", ")))
                .append(Doc::text("]")),

            Exp::Lambda { id, ref opt_guard, ref params, ref ret } =>
                Doc::text("fun")
                .append(Doc::space())
                .append(show_located_vec!(params, Doc::space()))
                .append(Doc::space())
                .append(show_located_opt!(opt_guard))
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(show_located_box!(ret)),
            Exp::For { id, ref generator, ref body } =>
                Doc::text("for")
                .append(Doc::space())
                .append(show_located_box!(generator))
                .append(Doc::space())
                .append(show_located_box!(body)),

            Exp::Ascribe { ref exp, ref pat } =>
                Doc::nil()
                .append(show_located_box!(exp))
                .append(Doc::space())
                .append(Doc::text("@"))
                .append(Doc::space())
                .append(show_located_box!(pat)),
            Exp::Arrow { id, ref arg, ref ret } =>
                Doc::nil()
                .append(show_located_box!(arg))
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(show_located_box!(ret)),
            Exp::Assign { ref lhs, ref rhs } =>
                Doc::nil()
                .append(show_located_box!(lhs))
                .append(Doc::space())
                .append(Doc::text(":="))
                .append(Doc::space())
                .append(show_located_box!(rhs)),
            Exp::Generator { ref lhs, ref rhs } =>
                Doc::nil()
                .append(show_located_box!(lhs))
                .append(Doc::space())
                .append(Doc::text("<-"))
                .append(Doc::space())
                .append(show_located_box!(rhs)),
            Exp::Bind { ref lhs, ref rhs } =>
                Doc::nil()
                .append(show_located_box!(lhs))
                .append(Doc::space())
                .append(Doc::text("="))
                .append(Doc::space())
                .append(show_located_box!(rhs)),

            Exp::Select { ref exp, ref name } =>
                Doc::nil()
                .append(show_located_box!(exp))
                .append(Doc::text("."))
                .append(name.to_doc()),
            Exp::Within { id, ref e1, ref e2 } =>
                Doc::nil()
                .append(show_located_box!(e1))
                .append(Doc::text("."))
                .append(show_located_box!(e2)),

            Exp::Apply { ref fun, ref arg } =>
                Doc::nil()
                .append(Doc::text("("))
                .append(show_located_box!(fun))
                .append(Doc::text(")"))
                .append(Doc::space())
                .append(show_located_box!(arg)),

            Exp::Lit { ref lit } =>
                lit.to_doc(),

            Exp::Name { ref name, .. } =>
                name.to_doc(),
            Exp::Unknown { ref name, .. } =>
                name.to_doc(),
            Exp::MixfixPart { ref name, .. } =>
                name.to_doc(),
            Exp::Var { ref name, .. } =>
                name.to_doc(),

            Exp::MixfixApply { ref es, .. } =>
                Doc::text("(")
                    .append(show_located_exp_vec!(es, Doc::space()))
                    .append(Doc::text(")")),
        }
    }
}

impl ToDoc for Param {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Param { attr: ParamAttr { ref assoc, ref by_name, ref mode }, ref pat } => {
                let pat1 = show_located_box!(pat);
                let (left1, right1) = match by_name {
                    CallingConv::ByValue => (Doc::text("("), Doc::text(")")),
                    CallingConv::ByName  => (Doc::text("{"), Doc::text("}")),
                };
                let (left, right) = match assoc {
                    Assoc::NonAssoc => (left1, right1),
                    Assoc::Assoc  => (left1.clone().append(left1), right1.clone().append(right1)),
                };
                let mode1 = match mode {
                    CallingMode::Input => Doc::text("?"),
                    CallingMode::Output => Doc::text("!"),
                };
                left.append(mode1).append(Doc::space()).append(pat1).append(right)
            }
        }
    }
}
