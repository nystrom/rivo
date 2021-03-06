use syntax::trees::*;
use syntax::loc::*;

// Macros for walking the AST.
#[macro_export]
macro_rules! walk_located {
    ($visitor: expr, $method: ident, $located: expr, $ctx: expr) => {
        Located {
            loc: $located.loc,
            value: $visitor.$method(&$located.value, $ctx, $located.loc)
        }
    };
}

#[macro_export]
macro_rules! walk_located_vec {
    ($visitor: expr, $method: ident, $list: expr, $ctx: expr) => {
        $list.iter().map(
            |located| walk_located!($visitor, $method, located, $ctx)
        ).collect()
    };
}

#[macro_export]
macro_rules! walk_located_box {
    ($visitor: expr, $method: ident, $located: expr, $ctx: expr) => {
        Box::new(walk_located!($visitor, $method, $located, $ctx))
    };
}

#[macro_export]
macro_rules! walk_located_opt {
    ($visitor: expr, $method: ident, $located: expr, $ctx: expr) => {
        match $located {
            Some(e) => Some(walk_located_box!($visitor, $method, e, $ctx)),
            None => None
        }
    };
}

/// Rewriter trait
/// 'a is the lifetime of the source tree.
/// Instances should override visit_xxx and leave walk_xxx alone.
pub trait Rewriter<'a, Ctx>: Sized {
    fn visit_root(&mut self, s: &'a Root, ctx: &Ctx, loc: Loc) -> Root {
        self.walk_root(s, ctx, loc)
    }

    fn visit_cmd(&mut self, s: &'a Cmd, ctx: &Ctx, loc: Loc) -> Cmd {
        self.walk_cmd(s, ctx, loc)
    }

    fn visit_def(&mut self, s: &'a Def, ctx: &Ctx, loc: Loc) -> Def {
        self.walk_def(s, ctx, loc)
    }

    fn visit_exp(&mut self, s: &'a Exp, ctx: &Ctx, loc: Loc) -> Exp {
        self.walk_exp(s, ctx, loc)
    }

    fn visit_param(&mut self, s: &'a Param, ctx: &Ctx, loc: Loc) -> Param {
        self.walk_param(s, ctx, loc)
    }

    fn walk_root(&mut self, s: &'a Root, ctx: &Ctx, loc: Loc) -> Root {
        match *s {
            Root::Bundle { id, ref cmds } => {
                Root::Bundle { id, cmds: walk_located_vec!(self, visit_cmd, &cmds, ctx) }
            },
        }
    }

    fn walk_cmd(&mut self, s: &'a Cmd, ctx: &Ctx, loc: Loc) -> Cmd {
        match *s {
            Cmd::Def(ref d) => {
                Cmd::Def(self.visit_def(&d, ctx, loc))
            },
            Cmd::Exp(ref e) => {
                Cmd::Exp(self.visit_exp(&e, ctx, loc))
            }
        }
    }

    fn walk_def(&mut self, s: &'a Def, ctx: &Ctx, loc: Loc) -> Def {
        match *s {
            Def::FormulaDef { ref attrs, ref flag, ref formula } => {
                Def::FormulaDef { attrs: attrs.clone(), flag: *flag, formula: walk_located_box!(self, visit_exp, formula, ctx) }
            },
            Def::TraitDef { id, ref attrs, ref name, ref opt_guard, ref params, ref supers, ref defs } => {
                Def::TraitDef { id, attrs: attrs.clone(), name: *name,
                    opt_guard: walk_located_opt!(self, visit_exp, opt_guard, ctx),
                    params: walk_located_vec!(self, visit_param, params, ctx),
                    supers: walk_located_vec!(self, visit_exp, supers, ctx),
                    defs: walk_located_vec!(self, visit_def, defs, ctx),
                 }
            },
            Def::FunDef { id, ref attrs, ref name, ref opt_guard, ref opt_body, ref params, ref ret } => {
                Def::FunDef { id, attrs: attrs.clone(), name: *name,
                    opt_guard: walk_located_opt!(self, visit_exp, opt_guard, ctx),
                    opt_body: walk_located_opt!(self, visit_exp, opt_body, ctx),
                    params: walk_located_vec!(self, visit_param, params, ctx),
                    ret: walk_located!(self, visit_param, ret, ctx),
                 }
            },
            Def::ImportDef { ref opt_path, ref selector } => {
                Def::ImportDef { opt_path: walk_located_opt!(self, visit_exp, opt_path, ctx), selector: selector.clone() }
            },
        }
    }

    fn walk_exp(&mut self, s: &'a Exp, ctx: &Ctx, loc: Loc) -> Exp {
        match *s {
            Exp::Layout { id, ref cmds } =>
                Exp::Layout { id, cmds: walk_located_vec!(self, visit_cmd, cmds, ctx) },

            Exp::AnyOf { ref es } =>
                Exp::AnyOf { es: walk_located_vec!(self, visit_exp, es, ctx) },
            Exp::TrySelect { ref exp, ref name } =>
                Exp::TrySelect { exp: walk_located_box!(self, visit_exp, exp, ctx), name: *name },
            Exp::OrElse { ref e1, ref e2 } =>
                Exp::OrElse { e1: walk_located_box!(self, visit_exp, e1, ctx), e2: walk_located_box!(self, visit_exp, e2, ctx) },
            Exp::Fail { ref message } =>
                Exp::Fail { message: message.clone() },
            Exp::Root =>
                Exp::Root,

            Exp::Tuple { ref es } =>
                Exp::Tuple { es: walk_located_vec!(self, visit_exp, es, ctx) },
            Exp::List { ref es } =>
                Exp::List { es: walk_located_vec!(self, visit_exp, es, ctx) },

            Exp::Lambda { id, ref opt_guard, ref params, ref ret } =>
                Exp::Lambda { id, opt_guard: walk_located_opt!(self, visit_exp, opt_guard, ctx), params: walk_located_vec!(self, visit_exp, params, ctx), ret: walk_located_box!(self, visit_exp, ret, ctx) },
            Exp::For { id, ref formula, ref body } =>
                Exp::For { id, formula: walk_located_box!(self, visit_exp, formula, ctx), body: walk_located_box!(self, visit_exp, body, ctx) },
            Exp::Let { id, ref formula, ref body } =>
                Exp::Let { id, formula: walk_located_box!(self, visit_exp, formula, ctx), body: walk_located_box!(self, visit_exp, body, ctx) },
            Exp::LetVar { id, ref formula, ref body } =>
                Exp::LetVar { id, formula: walk_located_box!(self, visit_exp, formula, ctx), body: walk_located_box!(self, visit_exp, body, ctx) },

            Exp::Arrow { id, ref arg, ref ret } =>
                Exp::Arrow { id, arg: walk_located_box!(self, visit_exp, arg, ctx), ret: walk_located_box!(self, visit_exp, ret, ctx) },
            Exp::Assign { ref lhs, ref rhs } =>
                Exp::Assign { lhs: walk_located_box!(self, visit_exp, lhs, ctx), rhs: walk_located_box!(self, visit_exp, rhs, ctx) },
            Exp::Bind { ref lhs, ref rhs } =>
                Exp::Bind { lhs: walk_located_box!(self, visit_exp, lhs, ctx), rhs: walk_located_box!(self, visit_exp, rhs, ctx) },
            Exp::Generator { ref lhs, ref rhs } =>
                Exp::Generator { lhs: walk_located_box!(self, visit_exp, lhs, ctx), rhs: walk_located_box!(self, visit_exp, rhs, ctx) },
            Exp::Where { ref pat, ref guard } =>
                Exp::Where { pat: walk_located_box!(self, visit_exp, pat, ctx), guard: walk_located_box!(self, visit_exp, guard, ctx) },

            Exp::Select { ref exp, ref name } =>
                Exp::Select { exp: walk_located_box!(self, visit_exp, exp, ctx), name: *name },
            Exp::Within { id, ref e1, ref e2 } =>
                Exp::Within { id, e1: walk_located_box!(self, visit_exp, e1, ctx), e2: walk_located_box!(self, visit_exp, e2, ctx) },
            Exp::Union { ref e1, ref e2 } =>
                Exp::Union { e1: walk_located_box!(self, visit_exp, e1, ctx), e2: walk_located_box!(self, visit_exp, e2, ctx) },
            Exp::Intersect { ref e1, ref e2 } =>
                Exp::Intersect { e1: walk_located_box!(self, visit_exp, e1, ctx), e2: walk_located_box!(self, visit_exp, e2, ctx) },

            Exp::Apply { ref fun, ref arg } =>
                Exp::Apply { fun: walk_located_box!(self, visit_exp, fun, ctx), arg: walk_located_box!(self, visit_exp, arg, ctx) },

            Exp::Lit { ref lit } =>
                Exp::Lit { lit: lit.clone() },

            Exp::Name { ref name, id } =>
                Exp::Name { name: *name, id },

            Exp::Unknown { ref name, id } =>
                Exp::Unknown { name: *name, id },
            Exp::MixfixPart { ref name, id } =>
                Exp::MixfixPart { name: *name, id },
            Exp::Var { ref name, id } =>
                Exp::Var { name: *name, id },

            Exp::MixfixApply { ref es, id } =>
                Exp::MixfixApply { es: walk_located_vec!(self, visit_exp, es, ctx), id },
        }
    }

    fn walk_param(&mut self, s: &'a Param, ctx: &Ctx, loc: Loc) -> Param {
        match *s {
            Param { ref attr, ref pat } => {
                let pat1 = walk_located!(self, visit_exp, pat, ctx);
                Param { attr: *attr, pat: Box::new(pat1) }
            }
        }
    }
}
