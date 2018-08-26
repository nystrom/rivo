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
}

impl ToDoc for Name {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Name::Id(ref x) => Doc::text(x),
            Name::Op(ref x) => Doc::text(x),
            Name::Mixfix(ref parts) =>
                Doc::text("`")
                .append(
                    Doc::intersperse(
                        parts.into_iter().map(
                            |part| part.to_doc()
                        ),
                        Doc::space()
                    )
                )
                .append(Doc::text("`")),
        }
    }
}

impl ToDoc for Part {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Part::Id(ref x) => Doc::text(x),
            Part::Op(ref x) => Doc::text(x),
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
            Root::Bundle { scope_id, ref cmds } =>
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
            Def::FormulaDef { ref flag, ref formula } => {
                let doc = match *flag {
                    FormulaFlag::Val => Doc::text("val"),
                    FormulaFlag::Var => Doc::text("var"),
                };
                doc.append(Doc::space())
                   .append(show_located_box!(formula))
            },
            Def::MixfixDef { scope_id, ref flag, ref name, ref opt_guard, ref params, ref ret } => {
                let doc = match *flag {
                    MixfixFlag::Trait => Doc::text("trait"),
                    MixfixFlag::Fun => Doc::text("fun"),
                };
                let arrow = match ret.value {
                    Param { mode: CallingMode::Output, .. } => Doc::text("->"),
                    Param { mode: CallingMode::Input, .. } => Doc::text("<-"),
                };
                doc.append(Doc::space())
                   .append(name.to_doc())
                   .append(show_located_vec!(params, Doc::space()))
                   .append(show_located_opt!(opt_guard))
                   .append(Doc::space())
                   .append(arrow)
                   .append(Doc::space())
                   .append(show_located!(ret))
            },
            Def::ImportDef { ref import } => {
                Doc::text("import")
                   .append(Doc::space())
                   .append(show_located_box!(import))
            },
        }
    }
}

impl ToDoc for Exp {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Exp::Layout { scope_id, ref cmds } =>
                Doc::text("{")
                .append(Doc::newline())
                .append(show_located_vec!(cmds, Doc::text(";").append(Doc::newline())).nest(1))
                .append(Doc::newline())
                .append(Doc::text("}")),
            Exp::Trait { scope_id, ref defs } =>
                Doc::text("{")
                .append(Doc::newline())
                .append(show_located_vec!(defs, Doc::text(";").append(Doc::newline())).nest(1))
                .append(Doc::newline())
                .append(Doc::text("}")),

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

            Exp::Lambda { scope_id, ref opt_guard, ref params, ref ret } =>
                Doc::text("fun")
                .append(Doc::space())
                .append(show_located_vec!(params, Doc::space()))
                .append(show_located_opt!(opt_guard))
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(show_located_box!(ret)),
            Exp::For { scope_id, ref generator, ref body } =>
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
            Exp::Arrow { ref arg, ref ret } =>
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
            Exp::Within { scope_id, ref e1, ref e2 } =>
                Doc::nil()
                .append(show_located_box!(e1))
                .append(Doc::text("."))
                .append(show_located_box!(e2)),

            Exp::Apply { ref fun, ref arg } =>
                Doc::nil()
                .append(show_located_box!(fun))
                .append(Doc::space())
                .append(show_located_box!(arg)),

            Exp::Lit { ref lit } =>
                lit.to_doc(),

            Exp::Native =>
                Doc::text("native"),
            Exp::Frame { scope_id } =>
                Doc::text("self"),

            Exp::Name { ref name, id } =>
                name.to_doc(),

            Exp::MixfixApply { ref es, id } =>
                Doc::text("(")
                    .append(show_located_vec!(es, Doc::space()))
                    .append(Doc::text(")")),
        }
    }
}

impl ToDoc for Param {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Param { assoc, by_name, mode, ref pat } => {
                let pat1 = show_located_box!(pat);
                let (left1, right1) = match by_name {
                    false => (Doc::text("("), Doc::text(")")),
                    true  => (Doc::text("{"), Doc::text("}")),
                };
                let (left, right) = match assoc {
                    false => (left1, right1),
                    true  => (left1.clone().append(left1), right1.clone().append(right1)),
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

impl ToDoc for Import {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            Import::All { ref path } => {
                path.to_doc()
                .append(Doc::text("."))
                .append(Doc::text("_"))
            }
            Import::None { ref path } => {
                path.to_doc()
                .append(Doc::text("."))
                .append(Doc::text("()"))
            }
            Import::Including { ref path, ref name } => {
                path.to_doc()
                .append(Doc::text("."))
                .append(name.to_doc())
            }
            Import::Excluding { ref path, ref name } => {
                path.to_doc()
                .append(Doc::text(".("))
                .append(name.to_doc())
                .append(Doc::text(" -> _)"))
            }
            Import::Renaming { ref path, ref name, ref rename } => {
                path.to_doc()
                .append(Doc::text(".("))
                .append(name.to_doc())
                .append(Doc::text(" -> "))
                .append(rename.to_doc()).append(Doc::text(")"))
            }
        }
    }
}
