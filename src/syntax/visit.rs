use syntax::trees::*;
use syntax::loc::*;

/// Rewriter trait
/// 's is the lifetime of the source tree
/// 't is the lifetime of the target tree
pub trait Rewriter<'a>: Sized {
    fn visit_root(&self, s: &'a Root) -> Root {
        walk_root(self, s)
    }

    fn visit_cmd(&self, s: &'a Cmd) -> Cmd {
        walk_cmd(self, s)
    }

    fn visit_def(&self, s: &'a Def) -> Def {
        walk_def(self, s)
    }

    fn visit_exp(&self, s: &'a Exp) -> Exp {
        walk_exp(self, s)
    }

    fn visit_param(&self, s: &'a Param) -> Param {
        walk_param(self, s)
    }

    fn visit_import(&self, s: &'a Import) -> Import {
        walk_import(self, s)
    }
}

#[macro_export]
macro_rules! walk_located {
    ($visitor: expr, $method: ident, $located: expr) => {
        Located { loc: $located.loc.clone(), value: $visitor.$method(&*$located) }
    };
}

#[macro_export]
macro_rules! walk_located_vec {
    ($visitor: expr, $method: ident, $list: expr) => {
        $list.into_iter().map(
            |located| walk_located!($visitor, $method, located)
        ).collect()
    };
}

#[macro_export]
macro_rules! walk_located_box {
    ($visitor: expr, $method: ident, $located: expr) => {
        Box::new(walk_located!($visitor, $method, $located))
    };
}

#[macro_export]
macro_rules! walk_located_opt {
    ($visitor: expr, $method: ident, $located: expr) => {
        match $located {
            Some(e) => Some(walk_located_box!($visitor, $method, e)),
            None => None
        }
    };
}

pub fn walk_root<'a, V: Rewriter<'a>>(v: &V, s: &'a Root) -> Root {
    match *s {
        Root::Bundle { scope_id, ref cmds } => {
            Root::Bundle { scope_id, cmds: walk_located_vec!(v, visit_cmd, &cmds) }
        },
    }
}

pub fn walk_cmd<'a, V: Rewriter<'a>>(v: &V, s: &'a Cmd) -> Cmd {
    match *s {
        Cmd::Def(ref d) => Cmd::Def(v.visit_def(&d)),
        Cmd::Exp(ref e) => Cmd::Exp(v.visit_exp(&e))
    }
}

pub fn walk_def<'a, V: Rewriter<'a>>(v: &V, s: &'a Def) -> Def {
    match *s {
        Def::FormulaDef { ref flag, ref formula } => {
            Def::FormulaDef { flag: *flag, formula: walk_located_box!(v, visit_exp, formula) }
        },
        Def::MixfixDef { scope_id, ref flag, ref name, ref opt_guard, ref params, ref ret } => {
            Def::MixfixDef { scope_id, flag: *flag, name: name.clone(), opt_guard: walk_located_opt!(v, visit_exp, opt_guard), params: walk_located_vec!(v, visit_param, params), ret: walk_located!(v, visit_param, ret) }
        },
        Def::ImportDef { ref import } => {
            Def::ImportDef { import: walk_located_box!(v, visit_exp, import) }
        },
    }

}
pub fn walk_exp<'a, V: Rewriter<'a>>(v: &V, s: &'a Exp) -> Exp {
    match *s {
        Exp::Layout { scope_id, ref cmds } =>
            Exp::Layout { scope_id, cmds: walk_located_vec!(v, visit_cmd, cmds) },
        Exp::Trait { scope_id, ref defs } =>
            Exp::Trait { scope_id, defs: walk_located_vec!(v, visit_def, defs) },

        Exp::Union { ref es } =>
            Exp::Union { es: walk_located_vec!(v, visit_exp, es) },
        Exp::Intersect { ref es } =>
            Exp::Intersect { es: walk_located_vec!(v, visit_exp, es) },

        Exp::Tuple { ref es } =>
            Exp::Tuple { es: walk_located_vec!(v, visit_exp, es) },
        Exp::List { ref es } =>
            Exp::List { es: walk_located_vec!(v, visit_exp, es) },

        Exp::Lambda { scope_id, ref opt_guard, ref params, ref ret } =>
            Exp::Lambda { scope_id, opt_guard: walk_located_opt!(v, visit_exp, opt_guard), params: walk_located_vec!(v, visit_exp, params), ret: walk_located_box!(v, visit_exp, ret) },
        Exp::For { scope_id, ref generator, ref body } =>
            Exp::For { scope_id, generator: walk_located_box!(v, visit_exp, generator), body: walk_located_box!(v, visit_exp, body) },

        Exp::Ascribe { ref exp, ref pat } =>
            Exp::Ascribe { exp: walk_located_box!(v, visit_exp, exp), pat: walk_located_box!(v, visit_exp, pat) },
        Exp::Arrow { ref arg, ref ret } =>
            Exp::Arrow { arg: walk_located_box!(v, visit_exp, arg), ret: walk_located_box!(v, visit_exp, ret) },
        Exp::Assign { ref lhs, ref rhs } =>
            Exp::Assign { lhs: walk_located_box!(v, visit_exp, lhs), rhs: walk_located_box!(v, visit_exp, rhs) },
        Exp::Generator { ref lhs, ref rhs } =>
            Exp::Generator { lhs: walk_located_box!(v, visit_exp, lhs), rhs: walk_located_box!(v, visit_exp, rhs) },
        Exp::Bind { ref lhs, ref rhs } =>
            Exp::Bind { lhs: walk_located_box!(v, visit_exp, lhs), rhs: walk_located_box!(v, visit_exp, rhs) },

        Exp::Select { ref exp, ref name } =>
            Exp::Select { exp: walk_located_box!(v, visit_exp, exp), name: name.clone() },
        Exp::Within { scope_id, ref e1, ref e2 } =>
            Exp::Within { scope_id, e1: walk_located_box!(v, visit_exp, e1), e2: walk_located_box!(v, visit_exp, e2) },

        Exp::Apply { ref fun, ref arg } =>
            Exp::Apply { fun: walk_located_box!(v, visit_exp, fun), arg: walk_located_box!(v, visit_exp, arg) },

        Exp::Lit { ref lit } =>
            Exp::Lit { lit: lit.clone() },

        Exp::Native =>
            Exp::Native,
        Exp::Frame { scope_id } =>
            Exp::Frame { scope_id },

        Exp::Name { ref name, id } =>
            Exp::Name { name: name.clone(), id },

        Exp::MixfixApply { ref es, id } =>
            Exp::MixfixApply { es: walk_located_vec!(v, visit_exp, es), id },
    }
}

pub fn walk_param<'a, V: Rewriter<'a>>(v: &V, s: &'a Param) -> Param {
    match *s {
        Param { assoc, by_name, mode, ref pat } => {
            let pat1 = walk_located!(v, visit_exp, pat);
            Param { assoc: assoc, by_name: by_name, mode: mode, pat: Box::new(pat1) }
        }
    }
}

pub fn walk_import<'a, V: Rewriter<'a>>(v: &V, s: &'a Import) -> Import {
    match *s {
        Import::All { ref path } => {
            Import::All { path: walk_located_box!(v, visit_exp, path) }
        }
        Import::None { ref path } => {
            Import::None { path: walk_located_box!(v, visit_exp, path) }
        }
        Import::Including { ref path, ref name } => {
            Import::Including { path: walk_located_box!(v, visit_exp, path), name: name.clone() }
        }
        Import::Excluding { ref path, ref name } => {
            Import::Excluding { path: walk_located_box!(v, visit_exp, path), name: name.clone() }
        }
        Import::Renaming { ref path, ref name, ref rename } => {
            Import::Renaming { path: walk_located_box!(v, visit_exp, path), name: name.clone(), rename: rename.clone() }
        }
    }
}
