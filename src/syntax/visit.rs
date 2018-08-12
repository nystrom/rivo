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
macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        $list.into_iter().map(
            |located| {
                let v = $visitor.$method(&located.value);
                Located { loc: located.loc.clone(), value: v }
            }
        ).collect()
    };
}

pub fn walk_root<'a, V: Rewriter<'a>>(v: &V, s: &'a Root) -> Root {
    match *s {
        Root::Parsed { ref cmds } => {
            let cmds1 = walk_list!(v, visit_cmd, &cmds);
            Root::Parsed { cmds: cmds1 }
        },
        Root::Prenamed { ref cmds, ref frame } => {
            let cmds1 = walk_list!(v, visit_cmd, &cmds);
            Root::Prenamed { frame: frame.clone(), cmds: cmds1 }
        },
        Root::Renamed { ref cmds, ref frame } => {
            let cmds1 = walk_list!(v, visit_cmd, &cmds);
            Root::Renamed { frame: frame.clone(), cmds: cmds1 }
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
            unimplemented!()
        },
        Def::MixfixDef { ref frame, ref flag, ref name, ref opt_guard, ref params, ref ret } => {
            unimplemented!()
        },
        Def::ImportDef { ref import } => {
            unimplemented!()
        },

        Def::AmbMixfixDef { ref flag, ref name, ref opt_guard, ref params, ref ret } => {
            unimplemented!()
            // let opt_guard1 = match opt_guard {
            //     Some(ref located) => {
            //         let Located { loc, value: e } = located;
            //         Some(Located { loc, value: v.visit_exp(e) })
            //     }
            //     None => None
            // };
            // let params1 = params.into_iter().map(
            //     |param| v.visit_param(param)
            // ).collect();
            // let ret1 = v.visit_param(ret);
            // Def::AmbMixfixDef { flag, name, opt_guard: Box(opt_guard1), params: params1, ret: ret1 }
        },

        Def::AmbImportDef { ref import } => {
            unimplemented!()
            // let Located { ref loc, value: ref e } = *import;
            // let import1 = Located { loc: *loc, value: v.visit_import(&e) };
            // Def::AmbImportDef { import: Box::new(import1) }
        },
    }

}
pub fn walk_exp<'a, V: Rewriter<'a>>(v: &V, s: &'a Exp) -> Exp {
    unimplemented!()
}
pub fn walk_param<'a, V: Rewriter<'a>>(v: &V, s: &'a Param) -> Param {
    unimplemented!()
}
pub fn walk_import<'a, V: Rewriter<'a>>(v: &V, s: &'a Import) -> Import {
    unimplemented!()
}
