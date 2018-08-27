use syntax::trees::*;
use syntax::loc::*;

// Macros for walking the AST.
macro_rules! walk_located {
    ($visitor: expr, $method: ident, $located: expr) => {
        Located { loc: $located.loc.clone(), value: $visitor.$method(&*$located) }
    };
}

macro_rules! walk_located_vec {
    ($visitor: expr, $method: ident, $list: expr) => {
        $list.into_iter().map(
            |located| walk_located!($visitor, $method, located)
        ).collect()
    };
}

macro_rules! walk_located_box {
    ($visitor: expr, $method: ident, $located: expr) => {
        Box::new(walk_located!($visitor, $method, $located))
    };
}

macro_rules! walk_located_opt {
    ($visitor: expr, $method: ident, $located: expr) => {
        match $located {
            Some(e) => Some(walk_located_box!($visitor, $method, e)),
            None => None
        }
    };
}

/// Rewriter trait
/// 'a is the lifetime of the source tree.
/// The protocol is as follows:
/// if enter_xxx returns Some(n), the new node is returned.
/// if enter_xxx returns None, the children are visited with walk_xxx,
/// creating a new node.
/// leave_xxx is called with the new node.
pub trait Rewriter<'a>: Sized {
    fn visit_root(&self, s: &'a Root) -> Root {
        match self.enter_root(s) {
            Some(s1) => {
                self.leave_root(s1)
            },
            None => {
                let s1 = self.walk_root(s);
                self.leave_root(s1)
            },
        }
    }

    fn enter_root(&self, s: &'a Root) -> Option<Root> {
        None
    }

    fn leave_root(&self, s: Root) -> Root {
        s
    }

    fn visit_cmd(&self, s: &'a Cmd) -> Cmd {
        match self.enter_cmd(s) {
            Some(s1) => {
                self.leave_cmd(s1)
            },
            None => {
                let s1 = self.walk_cmd(s);
                self.leave_cmd(s1)
            },
        }
    }

    fn enter_cmd(&self, s: &'a Cmd) -> Option<Cmd> {
        None
    }

    fn leave_cmd(&self, s: Cmd) -> Cmd {
        s
    }

    fn visit_def(&self, s: &'a Def) -> Def {
        match self.enter_def(s) {
            Some(s1) => {
                self.leave_def(s1)
            },
            None => {
                let s1 = self.walk_def(s);
                self.leave_def(s1)
            },
        }
    }

    fn enter_def(&self, s: &'a Def) -> Option<Def> {
        None
    }

    fn leave_def(&self, s: Def) -> Def {
        s
    }

    fn visit_exp(&self, s: &'a Exp) -> Exp {
        match self.enter_exp(s) {
            Some(s1) => {
                self.leave_exp(s1)
            },
            None => {
                let s1 = self.walk_exp(s);
                self.leave_exp(s1)
            },
        }
    }

    fn enter_exp(&self, s: &'a Exp) -> Option<Exp> {
        None
    }

    fn leave_exp(&self, s: Exp) -> Exp {
        s
    }

    fn visit_param(&self, s: &'a Param) -> Param {
        match self.enter_param(s) {
            Some(s1) => {
                self.leave_param(s1)
            },
            None => {
                let s1 = self.walk_param(s);
                self.leave_param(s1)
            },
        }
    }

    fn enter_param(&self, s: &'a Param) -> Option<Param> {
        None
    }

    fn leave_param(&self, s: Param) -> Param {
        s
    }

    fn visit_import(&self, s: &'a Import) -> Import {
        match self.enter_import(s) {
            Some(s1) => {
                self.leave_import(s1)
            },
            None => {
                let s1 = self.walk_import(s);
                self.leave_import(s1)
            },
        }
    }

    fn enter_import(&self, s: &'a Import) -> Option<Import> {
        None
    }

    fn leave_import(&self, s: Import) -> Import {
        s
    }

    fn walk_root(&self, s: &'a Root) -> Root {
        match *s {
            Root::Bundle { scope_id, ref cmds } => {
                Root::Bundle { scope_id, cmds: walk_located_vec!(self, visit_cmd, &cmds) }
            },
        }
    }

    fn walk_cmd(&self, s: &'a Cmd) -> Cmd {
        match *s {
            Cmd::Def(ref d) => Cmd::Def(self.visit_def(&d)),
            Cmd::Exp(ref e) => Cmd::Exp(self.visit_exp(&e))
        }
    }

    fn walk_def(&self, s: &'a Def) -> Def {
        match *s {
            Def::FormulaDef { ref flag, ref formula } => {
                Def::FormulaDef { flag: *flag, formula: walk_located_box!(self, visit_exp, formula) }
            },
            Def::MixfixDef { scope_id, ref flag, ref name, ref opt_guard, ref params, ref ret } => {
                Def::MixfixDef { scope_id, flag: *flag, name: name.clone(), opt_guard: walk_located_opt!(self, visit_exp, opt_guard), params: walk_located_vec!(self, visit_param, params), ret: walk_located!(self, visit_param, ret) }
            },
            Def::ImportDef { ref import } => {
                Def::ImportDef { import: walk_located_box!(self, visit_exp, import) }
            },
        }

    }
    fn walk_exp(&self, s: &'a Exp) -> Exp {
        match *s {
            Exp::Layout { scope_id, ref cmds } =>
                Exp::Layout { scope_id, cmds: walk_located_vec!(self, visit_cmd, cmds) },
            Exp::Trait { scope_id, ref defs } =>
                Exp::Trait { scope_id, defs: walk_located_vec!(self, visit_def, defs) },

            Exp::Union { ref es } =>
                Exp::Union { es: walk_located_vec!(self, visit_exp, es) },
            Exp::Intersect { ref es } =>
                Exp::Intersect { es: walk_located_vec!(self, visit_exp, es) },

            Exp::Tuple { ref es } =>
                Exp::Tuple { es: walk_located_vec!(self, visit_exp, es) },
            Exp::List { ref es } =>
                Exp::List { es: walk_located_vec!(self, visit_exp, es) },

            Exp::Lambda { scope_id, ref opt_guard, ref params, ref ret } =>
                Exp::Lambda { scope_id, opt_guard: walk_located_opt!(self, visit_exp, opt_guard), params: walk_located_vec!(self, visit_exp, params), ret: walk_located_box!(self, visit_exp, ret) },
            Exp::For { scope_id, ref generator, ref body } =>
                Exp::For { scope_id, generator: walk_located_box!(self, visit_exp, generator), body: walk_located_box!(self, visit_exp, body) },

            Exp::Ascribe { ref exp, ref pat } =>
                Exp::Ascribe { exp: walk_located_box!(self, visit_exp, exp), pat: walk_located_box!(self, visit_exp, pat) },
            Exp::Arrow { ref arg, ref ret } =>
                Exp::Arrow { arg: walk_located_box!(self, visit_exp, arg), ret: walk_located_box!(self, visit_exp, ret) },
            Exp::Assign { ref lhs, ref rhs } =>
                Exp::Assign { lhs: walk_located_box!(self, visit_exp, lhs), rhs: walk_located_box!(self, visit_exp, rhs) },
            Exp::Generator { ref lhs, ref rhs } =>
                Exp::Generator { lhs: walk_located_box!(self, visit_exp, lhs), rhs: walk_located_box!(self, visit_exp, rhs) },
            Exp::Bind { ref lhs, ref rhs } =>
                Exp::Bind { lhs: walk_located_box!(self, visit_exp, lhs), rhs: walk_located_box!(self, visit_exp, rhs) },

            Exp::Select { ref exp, ref name } =>
                Exp::Select { exp: walk_located_box!(self, visit_exp, exp), name: name.clone() },
            Exp::Within { scope_id, ref e1, ref e2 } =>
                Exp::Within { scope_id, e1: walk_located_box!(self, visit_exp, e1), e2: walk_located_box!(self, visit_exp, e2) },

            Exp::Apply { ref fun, ref arg } =>
                Exp::Apply { fun: walk_located_box!(self, visit_exp, fun), arg: walk_located_box!(self, visit_exp, arg) },

            Exp::Lit { ref lit } =>
                Exp::Lit { lit: lit.clone() },

            Exp::Native =>
                Exp::Native,
            Exp::Frame { scope_id } =>
                Exp::Frame { scope_id },

            Exp::Name { ref name, id } =>
                Exp::Name { name: name.clone(), id },

            Exp::MixfixApply { ref es, id } =>
                Exp::MixfixApply { es: walk_located_vec!(self, visit_exp, es), id },
        }
    }

    fn walk_param(&self, s: &'a Param) -> Param {
        match *s {
            Param { assoc, by_name, mode, ref pat } => {
                let pat1 = walk_located!(self, visit_exp, pat);
                Param { assoc: assoc, by_name: by_name, mode: mode, pat: Box::new(pat1) }
            }
        }
    }

    fn walk_import(&self, s: &'a Import) -> Import {
        match *s {
            Import::All { ref path } => {
                Import::All { path: walk_located_box!(self, visit_exp, path) }
            }
            Import::None { ref path } => {
                Import::None { path: walk_located_box!(self, visit_exp, path) }
            }
            Import::Including { ref path, ref name } => {
                Import::Including { path: walk_located_box!(self, visit_exp, path), name: name.clone() }
            }
            Import::Excluding { ref path, ref name } => {
                Import::Excluding { path: walk_located_box!(self, visit_exp, path), name: name.clone() }
            }
            Import::Renaming { ref path, ref name, ref rename } => {
                Import::Renaming { path: walk_located_box!(self, visit_exp, path), name: name.clone(), rename: rename.clone() }
            }
        }
    }
}
