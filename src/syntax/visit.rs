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
        Root::Parsed { ref cmds } => {
            Root::Parsed { cmds: walk_located_vec!(v, visit_cmd, &cmds) }
        },
        Root::Prenamed { ref cmds, ref frame } => {
            Root::Prenamed { frame: frame.clone(), cmds: walk_located_vec!(v, visit_cmd, &cmds) }
        },
        Root::Renamed { ref cmds, ref frame } => {
            Root::Renamed { frame: frame.clone(), cmds: walk_located_vec!(v, visit_cmd, &cmds) }
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
        Def::MixfixDef { ref frame, ref flag, ref name, ref opt_guard, ref params, ref ret } => {
            Def::MixfixDef { frame: frame.clone(), flag: *flag, name: name.clone(), opt_guard: walk_located_opt!(v, visit_exp, opt_guard), params: walk_located_vec!(v, visit_param, params), ret: walk_located!(v, visit_param, ret) }
        },
        Def::ImportDef { ref import } => {
            Def::ImportDef { import: walk_located_box!(v, visit_import, import) }
        },
        Def::AmbMixfixDef { ref flag, ref name, ref opt_guard, ref params, ref ret } => {
            Def::AmbMixfixDef { flag: *flag, name: name.clone(), opt_guard: walk_located_opt!(v, visit_exp, opt_guard), params: walk_located_vec!(v, visit_param, params), ret: walk_located!(v, visit_param, ret) }
        },
        Def::AmbImportDef { ref import } => {
            Def::AmbImportDef { import: walk_located_box!(v, visit_import, import) }
        },
    }

}
pub fn walk_exp<'a, V: Rewriter<'a>>(v: &V, s: &'a Exp) -> Exp {
    match *s {
        Exp::Layout { ref cmds, ref frame } =>
            Exp::Layout { cmds: walk_located_vec!(v, visit_cmd, cmds), frame: frame.clone() },
        Exp::Trait { ref cmds, ref frame } =>
            Exp::Trait { cmds: walk_located_vec!(v, visit_cmd, cmds), frame: frame.clone() },

        Exp::Union { ref es } =>
            Exp::Union { es: walk_located_vec!(v, visit_exp, es) },

        Exp::Intersect { ref es } =>
            Exp::Intersect { es: walk_located_vec!(v, visit_exp, es) },

        Exp::Lambda { ref frame, ref opt_guard, ref params, ref ret } =>
            Exp::Lambda { frame: frame.clone(), opt_guard: walk_located_opt!(v, visit_exp, opt_guard), params: walk_located_vec!(v, visit_exp, params), ret: walk_located_box!(v, visit_exp, ret) },
        Exp::For { ref frame, ref generator, ref body } =>
            Exp::For { frame: frame.clone(), generator: walk_located_box!(v, visit_exp, generator), body: walk_located_box!(v, visit_exp, body) },

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
        Exp::Apply { ref fun, ref arg } =>
            Exp::Apply { fun: walk_located_box!(v, visit_exp, fun), arg: walk_located_box!(v, visit_exp, arg) },

        Exp::Var { ref  name, ref decls } =>
            Exp::Var { name: name.clone(), decls: decls.clone() },
        Exp::Unknown { ref  name, ref decls } =>
            Exp::Unknown { name: name.clone(), decls: decls.clone() },
        Exp::MixfixPart { ref  name, ref decls } =>
            Exp::MixfixPart { name: name.clone(), decls: decls.clone() },

        Exp::Literal { ref lit } =>
            Exp::Literal { lit: lit.clone() },

        Exp::Native =>
            Exp::Native,
        Exp::GlobalFrame =>
            Exp::GlobalFrame,
        Exp::CurrentFrame { ref scope } =>
            Exp::CurrentFrame { scope: scope.clone() },

        Exp::AmbLayout { ref cmds } =>
            Exp::AmbLayout { cmds: walk_located_vec!(v, visit_cmd, cmds) },
        Exp::AmbTrait { ref cmds } =>
            Exp::AmbTrait { cmds: walk_located_vec!(v, visit_cmd, cmds) },
        Exp::AmbFrame =>
            Exp::AmbFrame,
        Exp::AmbLambda { ref opt_guard, ref params, ref ret } =>
            Exp::AmbLambda { opt_guard: walk_located_opt!(v, visit_exp, opt_guard), params: walk_located_vec!(v, visit_exp, params), ret: walk_located_box!(v, visit_exp, ret) },
        Exp::AmbFor { ref generator, ref body } =>
            Exp::AmbFor { generator: walk_located_box!(v, visit_exp, generator), body: walk_located_box!(v, visit_exp, body) },
        Exp::AmbWithin { ref e1, ref e2 } =>
            Exp::AmbWithin { e1: walk_located_box!(v, visit_exp, e1), e2: walk_located_box!(v, visit_exp, e2) },

        Exp::AmbUnion { ref es } =>
            Exp::AmbUnion { es: walk_located_vec!(v, visit_exp, es) },

        Exp::AmbName { ref name } =>
            Exp::AmbName { name: name.clone() },

        Exp::MixfixApply { ref es } =>
            Exp::MixfixApply { es: walk_located_vec!(v, visit_exp, es) },

        Exp::VarRef { ref name, ref lookup_ref } =>
            Exp::VarRef { name: name.clone(), lookup_ref: lookup_ref.clone() },
        Exp::MixfixRef { ref name, ref lookup_ref } =>
            Exp::MixfixRef { name: name.clone(), lookup_ref: lookup_ref.clone() },
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
