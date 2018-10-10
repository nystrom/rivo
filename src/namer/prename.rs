use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;

use namer::symbols::*;
use namer::graph::*;
use namer::glr::*;
use driver;

use visit::rewrite::*;

use std::collections::HashMap;

// First pass:
// Create the scopes and initialize in the AST.
// Second pass: do the declarations, etc. Then call ScopeGraph.solve

#[derive(Clone)]
pub struct PrenameContext {
    scope: Scope,
    import_scope: Scope,
    names: Vec<Name>,
    unknowns: Vec<Located<Decl>>,
    in_mixfix: bool,
    prio: Prio,
}

impl PrenameContext {
    // The initial environment is EmptyEnv, not GlobalEnv.
    // When we enter the Bundle during the tree traversal,
    // we should have imports from GlobalFrame, which will
    // bring in the GlobalEnv if not excluded by an `import ()`
    pub fn new() -> PrenameContext {
        PrenameContext {
            scope: Scope::Empty,
            import_scope: Scope::Empty,
            names: vec![],
            unknowns: vec![],
            in_mixfix: false,
            prio: Prio(0)
        }
    }
}

impl<'a> Prenamer<'a> {
    pub fn get_unknowns_from_params(flag: FormulaFlag, params: &Vec<Located<Param>>, defined_names: &Vec<Name>, scope: Scope) -> Vec<Located<Decl>> {
        let mut decls = Vec::new();
        for Located { loc, value: Param { pat: e, .. } } in params {
            Prenamer::add_unknowns_for_exp(&mut decls, flag, e, defined_names, scope);
        }
        decls
    }

    pub fn get_unknowns(flag: FormulaFlag, params: &Vec<Located<Exp>>, defined_names: &Vec<Name>, scope: Scope) -> Vec<Located<Decl>> {
        let mut decls = Vec::new();
        for e in params {
            Prenamer::add_unknowns_for_exp(&mut decls, flag, e, defined_names, scope);
        }
        decls
    }

    pub fn get_unknowns_from_exp(flag: FormulaFlag, e: &Located<Exp>, defined_names: &Vec<Name>, scope: Scope) -> Vec<Located<Decl>> {
        let mut decls = Vec::new();
        Prenamer::add_unknowns_for_exp(&mut decls, flag, e, defined_names, scope);
        decls
    }

    fn add_unknown_for_name(decls: &mut Vec<Located<Decl>>, flag: FormulaFlag, name: Name, defined_names: &Vec<Name>, scope: Scope, loc: &Loc) {
        for decl in decls.iter() {
            if decl.name() == name {
                // already declared here
                return;
            }
        }
        for defined in defined_names {
            if *defined == name {
                // already declared in outer scope
                return;
            }
        }

        let decl = match flag {
            FormulaFlag::Val => Decl::Val { scope, name },
            FormulaFlag::Var => Decl::Var { scope, name },
        };

        decls.push(Located { loc: loc.clone(), value: decl });
    }

    fn add_unknowns_for_exp(decls: &mut Vec<Located<Decl>>, flag: FormulaFlag, e: &Located<Exp>, defined_names: &Vec<Name>, scope: Scope) {
        match &e.value {
            Exp::Union { es } => {
                for e in es {
                    Prenamer::add_unknowns_for_exp(decls, flag, &e, defined_names, scope);
                }
            },
            Exp::Intersect { es } => {
                for e in es {
                    Prenamer::add_unknowns_for_exp(decls, flag, &e, defined_names, scope);
                }
            },
            Exp::Tuple { es } => {
                for e in es {
                    Prenamer::add_unknowns_for_exp(decls, flag, &e, defined_names, scope);
                }
            },
            Exp::List { es } => {
                for e in es {
                    Prenamer::add_unknowns_for_exp(decls, flag, &e, defined_names, scope);
                }
            },
            Exp::Ascribe { exp, pat } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &exp, defined_names, scope);
                Prenamer::add_unknowns_for_exp(decls, flag, &pat, defined_names, scope);
            },
            Exp::Assign { lhs, rhs } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &lhs, defined_names, scope);
                Prenamer::add_unknowns_for_exp(decls, flag, &rhs, defined_names, scope);
                // Names on the left of a binding are also unknowns, regardless of their case.
                match **lhs {
                    Located { ref loc, value: Exp::Name { ref id, ref name } } => {
                        Prenamer::add_unknown_for_name(decls, flag, name.clone(), defined_names, scope, &e.loc);
                    },
                    _ => {},
                }
            },
            Exp::Generator { lhs, rhs } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &lhs, defined_names, scope);
                Prenamer::add_unknowns_for_exp(decls, flag, &rhs, defined_names, scope);
                // Names on the left of a binding are also unknowns, regardless of their case.
                match **lhs {
                    Located { ref loc, value: Exp::Name { ref id, ref name } } => {
                        Prenamer::add_unknown_for_name(decls, flag, name.clone(), defined_names, scope, &e.loc);
                    },
                    _ => {},
                }
            },
            Exp::Bind { lhs, rhs } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &lhs, defined_names, scope);
                Prenamer::add_unknowns_for_exp(decls, flag, &rhs, defined_names, scope);
                // Names on the left of a binding are also unknowns, regardless of their case.
                match **lhs {
                    Located { ref loc, value: Exp::Name { ref id, ref name } } => {
                        Prenamer::add_unknown_for_name(decls, flag, name.clone(), defined_names, scope, &e.loc);
                    },
                    _ => {},
                }
            },
            Exp::Select { exp, name } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &exp, defined_names, scope);
            },
            Exp::Within { id, e1, e2 } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &e1, defined_names, scope);
            },
            Exp::Apply { fun, arg } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &fun, defined_names, scope);
                Prenamer::add_unknowns_for_exp(decls, flag, &arg, defined_names, scope);
            },
            Exp::Name { id, name } => {
                Prenamer::add_unknown_for_name(decls, flag, name.clone(), defined_names, scope, &e.loc);
            },
            Exp::MixfixApply { es, id } => {
                for e in es {
                    Prenamer::add_unknowns_for_exp(decls, flag, &e, defined_names, scope);
                }
            },
            _ => {},
        }
    }

    pub fn get_imported_names(cmds: &Vec<Located<Cmd>>) -> Vec<Name> {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: loc.clone(), value: d.clone() }),
                _ => None,
            }
        );
        Prenamer::get_imported_names_from_defs(defs)
    }

    pub fn get_declared_names(cmds: &Vec<Located<Cmd>>) -> Vec<Name> {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: loc.clone(), value: d.clone() }),
                _ => None,
            }
        );
        Prenamer::get_declared_names_from_defs(defs)
    }

    fn add_imported_names(namespace: &Located<Exp>, names: &mut Vec<Name>) {
        match namespace {
            Located { loc, value: Exp::Name { id, name } } => {
                names.push(name.clone());
            },
            Located { loc, value: Exp::Select { exp, name } } => {
                names.push(name.clone());
            },
            Located { loc, value: Exp::Union { es } } => {
                for e in es {
                    Prenamer::add_imported_names(e, names);
                }
            },
            Located { loc, value: Exp::Tuple { es } } => {
                for e in es {
                    Prenamer::add_imported_names(e, names);
                }
            },
            Located { loc, value: Exp::Within { id, e1, e2 } } => {
                Prenamer::add_imported_names(&**e2, names);
            },
            Located { loc, value: Exp::Arrow { id, arg, ret } } => {
                Prenamer::add_imported_names(&**ret, names);
            },
            _ => {
                // bad import!
            },
        }

    }

    pub fn get_imported_names_from_defs<T>(defs: T) -> Vec<Name>
        where T : IntoIterator<Item = Located<Def>>
    {
        let mut names = Vec::new();
        for def in defs {
            match def {
                Located { loc, value: Def::ImportDef { import } } => {
                    Prenamer::add_imported_names(&*import, &mut names);
                },
                _ => {},
            }
        }

        names
    }

    pub fn get_declared_names_from_defs<T>(defs: T) -> Vec<Name>
        where T : IntoIterator<Item = Located<Def>>
    {
        let mut names = Vec::new();
        for def in defs {
            match def {
                Located { loc, value: Def::MixfixDef { name, .. } } => {
                    names.push(name);
                },
                // Located { loc, value: Def::GroupDef { defs, .. } } => {
                //     let mut nested = Prenamer::get_declared_names_from_defs(defs);
                //     names.append(&mut nested);
                // },
                _ => {},
            }
        }

        names
    }

    pub fn add_imports(&mut self, import_into_scope: Scope, lookup_scope: Scope, cmds: &Vec<Located<Cmd>>) {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: loc.clone(), value: d.clone() }),
                _ => None,
            }
        );
        self.add_imports_from_defs(import_into_scope, lookup_scope, defs);
    }

    pub fn add_imports_from_defs<T>(&mut self, import_into_scope: Scope, lookup_scope: Scope, defs: T)
        where T : Iterator<Item = Located<Def>>
    {
        for def in defs {
            match def {
                Located { loc, value: Def::ImportDef { import } } => {
                    self.add_import(import_into_scope, lookup_scope, &*import);
                },
                _ => {},
            }
        }
    }

    pub fn add_import(&mut self, import_into_scope: Scope, lookup_scope: Scope, namespace: &Located<Exp>) {
        // We've delayed parsing of imports until this point.
        // We have to check that all the imports are valid stable paths.
        // But note that we have not yet rewritten mixfix expressions into apply expressions.
        // FIXME: add a check in ScopeGraph for stable paths.
        match namespace {
            Located { loc, value: Exp::Lit { lit: Lit::Wildcard } } => {
                self.graph.import(import_into_scope, &Located { loc: loc.clone(), value: Import::All { path: lookup_scope.clone() } });
            },
            Located { loc, value: Exp::Lit { lit: Lit::Nothing } } => {
                self.graph.import(import_into_scope, &Located { loc: loc.clone(), value: Import::None { path: lookup_scope.clone() } });
            },
            Located { loc, value: Exp::Select { exp, name } } => {
                let scope = self.lookup_frame(lookup_scope, &*exp);
                self.graph.import(import_into_scope, &Located { loc: loc.clone(), value: Import::Including { path: scope, name: name.clone() } });
            },
            Located { loc, value: Exp::Name { id, name } } => {
                self.graph.import(import_into_scope, &Located { loc: loc.clone(), value: Import::Including { path: lookup_scope.clone(), name: name.clone() } });
            },
            Located { loc, value: Exp::Union { es } } => {
                for e in es {
                    self.add_import(import_into_scope, lookup_scope, e);
                }
            },
            Located { loc, value: Exp::Tuple { es } } => {
                for e in es {
                    self.add_import(import_into_scope, lookup_scope, e);
                }
            },
            Located { loc, value: Exp::Within { id, e1, e2 } } => {
                let scope = self.lookup_frame(lookup_scope, &*e1);
                self.add_import(import_into_scope, scope, &*e2);
            },
            Located { loc, value: Exp::Arrow { id, arg, ret } } => {
                match &**arg {
                    Located { loc: _, value: Exp::Name { id: _, name } } => {
                        match &**ret {
                            Located { loc: _, value: Exp::Name { id: _, name: rename } } => {
                                self.graph.import(import_into_scope, &Located { loc: loc.clone(), value: Import::Renaming { path: lookup_scope.clone(), name: name.clone(), rename: rename.clone() } });
                            },
                            Located { loc: _, value: Exp::Lit { lit: Lit::Nothing } } => {
                                self.graph.import(import_into_scope, &Located { loc: loc.clone(), value: Import::Excluding { path: lookup_scope.clone(), name: name.clone() } });
                            },
                            _ => {
                                // bad import!
                            },
                        }
                    },
                    _ => {
                        // bad import!
                    },
                }
            },
            _ => {
                // bad import!
            },
        }
    }



    pub fn lookup_here(&mut self, id: NodeId, scope: Scope, name: Name) -> LookupHereIndex {
        if let Some(lookup) = self.lookups_here.get(&id) {
            *lookup
        }
        else {
            let lookup = self.graph.lookup_here(scope, name);
            self.lookups_here.insert(id, lookup);
            lookup
        }
    }

    pub fn lookup(&mut self, id: NodeId, scope: Scope, name: Name) -> LookupIndex {
        if let Some(lookup) = self.lookups.get(&id) {
            *lookup
        }
        else {
            let lookup = self.graph.lookup(scope, name);
            self.lookups.insert(id, lookup);
            lookup
        }
    }

    pub fn parse_mixfix(&mut self, id: NodeId, scope: Scope, es: &Vec<Located<Exp>>) -> MixfixIndex {
        if let Some(lookup) = self.mixfixes.get(&id) {
            *lookup
        }
        else {
            let parts = es.iter().map(|e|
                match e.value {
                    Exp::Name { ref name, id } => {
                        let lookup = self.lookup(id, scope, name.clone());
                        MixfixPart { name_ref: Some(lookup) }
                    },
                    _ =>
                        MixfixPart { name_ref: None }
                }
            ).collect();
            let lookup = self.graph.parse_mixfix(parts);
            self.mixfixes.insert(id, lookup);
            lookup
        }
    }

    pub fn lookup_frame(&mut self, scope: Scope, e: &Located<Exp>) -> Scope {
        match &e.value {
            Exp::Frame { id } => {
                let scope = self.scopes.get(&id);
                *scope.unwrap_or(&Scope::Empty)
            }
            Exp::Name { name, id } => {
                let lookup = self.lookup_here(*id, scope, name.clone());
                self.graph.get_scope_of_lookup_here(lookup)
            },
            Exp::MixfixApply { es, id } => {
                let lookup = self.parse_mixfix(*id, scope, es);
                self.graph.get_scope_of_mixfix(lookup)
            },
            Exp::Apply { fun, arg } => {
                // When looking up (`List _` Int) it's sufficient to just lookup
                // `List _`. The `Int` will be substituted in at runtime.
                self.lookup_frame(scope, &*fun)
            },
            Exp::Union { es } => {
                let env = self.graph.new_env();
                for e in es {
                    let s = self.lookup_frame(scope, e);
                    self.graph.include(env, s);
                }
                env
            },
            _ => {
                Scope::Empty
            },
        }
    }
}

pub struct Prenamer<'a> {
    pub graph: &'a mut ScopeGraph,
    pub scopes: &'a mut HashMap<NodeId, Scope>,
    pub lookups: &'a mut HashMap<NodeId, LookupIndex>,
    pub lookups_here: &'a mut HashMap<NodeId, LookupHereIndex>,
    pub mixfixes: &'a mut HashMap<NodeId, MixfixIndex>,
    pub driver: &'a mut driver::Interpreter,
}

impl<'a> Prenamer<'a> {
    fn walk_cmd_list(&mut self, body: &Vec<Located<Cmd>>, ctx: &PrenameContext) -> Vec<Located<Cmd>> {
        let mut result = Vec::new();
        let prio = 0;
        let mut new_names = ctx.names.clone();

        for cmd in body {
            let ctx1 = PrenameContext {
                prio: Prio(prio),
                names: new_names.clone(),
                ..ctx.clone()
            };

            match cmd {
                Located { loc, value: Cmd::Def(Def::FormulaDef { flag, formula }) } => {
                    let names = &ctx1.names;
                    let unknowns = Prenamer::get_unknowns_from_exp(*flag, &*formula, names, ctx1.scope);

                    for decl in &unknowns {
                        new_names.push(decl.name());
                        self.graph.declare(ctx1.scope, decl);
                    }

                    let cmd1 = self.visit_cmd(&cmd.value, &ctx1, &cmd.loc);
                    result.push(Located { loc: cmd.loc.clone(), value: cmd1 });
                },
                _ => {
                    let cmd1 = self.visit_cmd(&cmd.value, &ctx1, &cmd.loc);
                    result.push(Located { loc: cmd.loc.clone(), value: cmd1 });
                },
            }
        }
        result
    }

    fn walk_def_list(&mut self, body: &Vec<Located<Def>>, ctx: &PrenameContext) -> Vec<Located<Def>> {
        let mut result = Vec::new();
        let prio = 0;
        let mut new_names = ctx.names.clone();

        for def in body {
            let ctx1 = PrenameContext {
                prio: Prio(prio),
                names: new_names.clone(),
                ..ctx.clone()
            };

            match def {
                Located { loc, value: Def::FormulaDef { flag, formula } } => {
                    let names = &ctx1.names;
                    let unknowns = Prenamer::get_unknowns_from_exp(*flag, &*formula, names, ctx1.scope);

                    for decl in &unknowns {
                        new_names.push(decl.name());
                        self.graph.declare(ctx1.scope, decl);
                    }

                    let def1 = self.visit_def(&def.value, &ctx1, &def.loc);
                    result.push(Located { loc: def.loc.clone(), value: def1 });
                },
                _ => {
                    let def1 = self.visit_def(&def.value, &ctx1, &def.loc);
                    result.push(Located { loc: def.loc.clone(), value: def1 });
                },
            }
        }
        result
    }
}

// visit any node that has a node id.
impl<'tables, 'a> Rewriter<'a, PrenameContext> for Prenamer<'tables> {
    fn visit_root(&mut self, s: &'a Root, ctx: &PrenameContext, loc: &Loc) -> Root {
        match s {
            Root::Bundle { id, cmds } => {
                let scope = self.graph.new_env();
                self.graph.set_parent(scope, Scope::Global);
                self.scopes.insert(*id, scope);

                // frame for resolving imports
                let import_scope = self.graph.new_env();
                self.graph.set_parent(import_scope, Scope::Global);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names(cmds);
                let decl_names = Prenamer::get_declared_names(cmds);

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameContext {
                    scope,
                    import_scope,
                    names: new_names,
                    ..ctx.clone()
                };

                let new_node = Root::Bundle {
                    id: *id,
                    cmds: self.walk_cmd_list(&cmds, &child_ctx)
                };

                // Include nested records in the scope.
                match new_node {
                    Root::Bundle { ref cmds, .. } => {
                        for cmd in cmds {
                            match cmd {
                                Located {
                                    loc,
                                    value: Cmd::Exp(Exp::Record { id, .. })
                                } => {
                                    if let Some(inner_scope) = self.scopes.get(&id) {
                                        self.graph.include(scope, *inner_scope);
                                    }
                                },
                                _ => {},
                            }
                        }

                        self.add_imports(scope, import_scope, cmds);
                    },
                }

                new_node
            },
        }
    }

    fn visit_def(&mut self, s: &'a Def, ctx: &PrenameContext, loc: &Loc) -> Def {
        match s {
            Def::MixfixDef { id, flag, name, opt_guard, params, ret } => {
                let scope = self.graph.new_env();
                self.scopes.insert(*id, scope);

                self.graph.set_parent(scope, ctx.scope);

                let mut unknowns = Vec::new();

                for Located { loc, value: Param { pat, mode, .. } } in params {
                    match mode {
                        CallingMode::Input => {
                            Prenamer::add_unknowns_for_exp(&mut unknowns, FormulaFlag::Val, pat, &ctx.names, scope);
                        },
                        _ => {},
                    }
                }

                let mut new_names = ctx.names.clone();

                for decl in &unknowns {
                    new_names.push(decl.name());
                    self.graph.declare(scope, decl);
                }

                let child_ctx = PrenameContext {
                    scope,
                    import_scope: Scope::Empty,
                    names: new_names.clone(),
                    ..ctx.clone()
                };

                let input_params_ctx = PrenameContext {
                    unknowns,
                    ..child_ctx.clone()
                };

                let opt_guard1 = opt_guard.clone().map(|x|
                    match *x {
                        Located { loc, value: e } => Box::new(Located { loc: loc.clone(), value: self.visit_exp(&e, &child_ctx, &loc) })
                    }
                );

                let ret1 = match ret {
                    Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Input } } => {
                        Located { loc: ret.loc.clone(), value:
                            Param { pat: Box::new(Located { loc: e.loc.clone(), value: self.visit_exp(&e.value, &input_params_ctx, &e.loc) }), assoc: *assoc, by_name: *by_name, mode: CallingMode::Input } }
                    },
                    Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Output } } => {
                        Located { loc: ret.loc.clone(), value:
                            Param { pat: Box::new(Located { loc: e.loc.clone(), value: self.visit_exp(&e.value, &child_ctx, &e.loc) }), assoc: *assoc, by_name: *by_name, mode: CallingMode::Output } }
                    },
                };

                let mut params1 = Vec::new();

                for param in params {
                    match param {
                        Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Input } } => {
                            let param1 = Located { loc: ret.loc.clone(), value:
                                Param { pat: Box::new(Located { loc: e.loc.clone(), value: self.visit_exp(&e.value, &input_params_ctx, &e.loc) }), assoc: *assoc, by_name: *by_name, mode: CallingMode::Input } };
                            params1.push(param1);
                        },
                        Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Output } } => {
                            let param1 = Located { loc: ret.loc.clone(), value:
                                Param { pat: Box::new(Located { loc: e.loc.clone(), value: self.visit_exp(&e.value, &child_ctx, &e.loc) }), assoc: *assoc, by_name: *by_name, mode: CallingMode::Output } };
                            params1.push(param1);
                        },
                    }
                }

                let param_assocs = params.iter().map(|Located { value: Param { assoc, .. }, .. }| *assoc).collect();
                let param_convs = params.iter().map(|Located { value: Param { by_name, .. }, .. }| *by_name).collect();
                let param_modes = params.iter().map(|Located { value: Param { mode, .. }, .. }| *mode).collect();
                let ret_mode = match ret {
                    Located { loc: _, value: Param { mode, .. } } => mode
                };
                let ret_conv = match ret {
                    Located { loc: _, value: Param { by_name, .. } } => by_name
                };
                let prio = ctx.prio;

                let decl = match flag {
                    MixfixFlag::Fun =>
                        Located {
                            loc: loc.clone(),
                            value: Decl::Fun {
                                scope: ctx.scope,
                                name: name.clone(),
                                param_assocs,
                                param_convs,
                                param_modes,
                                ret_mode: *ret_mode,
                                ret_conv: *ret_conv,
                                prio,
                            }
                        },
                    MixfixFlag::Trait =>
                        Located {
                            loc: loc.clone(),
                            value: Decl::Trait {
                                scope: ctx.scope,
                                name: name.clone(),
                                param_assocs,
                                param_convs,
                                param_modes,
                                ret_mode: *ret_mode,
                                ret_conv: *ret_conv,
                                prio,
                                body: vec![]
                            }
                        },

                };

                self.graph.declare(ctx.scope, &decl);
                self.graph.declare(ctx.import_scope, &decl);

                // TODO: declare mixfix parts
                match name {
                    Name::Mixfix(parts) => {
                        for (i, part) in parts.iter().enumerate() {
                            match part {
                                Part::Id(x) => {
                                    let mixfix_decl = Located { loc: loc.clone(), value: Decl::MixfixPart { name: Name::Id(x.clone()), index: i, full: name.clone(), orig: Box::new(decl.value.clone()) } };
                                    self.graph.declare(ctx.scope, &mixfix_decl);
                                    self.graph.declare(ctx.import_scope, &mixfix_decl);
                                },
                                Part::Op(x) => {
                                    let mixfix_decl = Located { loc: loc.clone(), value: Decl::MixfixPart { name: Name::Op(x.clone()), index: i, full: name.clone(), orig: Box::new(decl.value.clone()) } };
                                    self.graph.declare(ctx.scope, &mixfix_decl);
                                    self.graph.declare(ctx.import_scope, &mixfix_decl);
                                },
                                _ => {},
                            }
                        }
                    },
                    _ => {},
                }

                Def::MixfixDef { id: *id, flag: *flag, name: name.clone(), opt_guard: opt_guard1, params: params1, ret: ret1 }
            },
            Def::FormulaDef { flag, formula } => {
                self.walk_def(s, ctx, loc)
            },
            Def::ImportDef { import } => {
                // Swap the environment with the weaker import env.
                // This prevents import resolution from searching the frame with
                // the imports themselves.

                let child_ctx = PrenameContext {
                    scope: ctx.import_scope,
                    import_scope: Scope::Empty,
                    ..ctx.clone()
                };

                self.walk_def(s, &child_ctx, loc)
            },
        }
    }

    fn visit_exp(&mut self, s: &'a Exp, ctx: &PrenameContext, loc: &Loc) -> Exp {
        match s {
            Exp::Layout { id, cmds } => {
                let scope = self.graph.new_env();
                self.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                // frame for resolving imports
                let import_scope = self.graph.new_env();
                self.graph.set_parent(import_scope, ctx.scope);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names(cmds);
                let decl_names = Prenamer::get_declared_names(cmds);

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameContext {
                    scope,
                    import_scope,
                    names: new_names,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(s, &child_ctx, &loc);

                // Include nested records in the scope.
                match new_node {
                    Exp::Layout { ref cmds, .. } => {
                        for cmd in cmds {
                            match cmd {
                                Located {
                                    loc,
                                    value: Cmd::Exp(Exp::Record { id, .. })
                                } => {
                                    if let Some(inner_scope) = self.scopes.get(&id) {
                                        self.graph.include(scope, *inner_scope);
                                    }
                                },
                                _ => {},
                            }
                        }

                        self.add_imports(scope, import_scope, cmds);
                    },
                    _ => {
                        unimplemented!()
                    },
                }

                new_node
            },
            Exp::Record { id, defs } => {
                let scope = self.graph.new_env();
                self.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                // frame for resolving imports
                let import_scope = self.graph.new_env();
                self.graph.set_parent(import_scope, ctx.scope);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names_from_defs(defs.iter().cloned());
                let decl_names = Prenamer::get_declared_names_from_defs(defs.iter().cloned());

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameContext {
                    scope,
                    import_scope,
                    names: new_names,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(s, &child_ctx, &loc);

                new_node
            },
            Exp::Union { .. } => {
                let scope = ctx.scope;
                let new_node = self.walk_exp(s, &ctx, &loc);

                // FIXME: remove this clone
                let scopes = self.scopes.clone();

                // Import the other frames into the traits in the union.
                match &new_node {
                    Exp::Union { es } => {
                        for (i, ei) in es.iter().enumerate() {
                            match ei.value {
                                Exp::Record { id, .. } => {
                                    if let Some(scope_i) = scopes.get(&id) {
                                        for (j, ej) in es.iter().enumerate() {
                                            if i != j {
                                                let scope_j = self.lookup_frame(scope, ej);
                                                self.graph.import(*scope_i, &Located::new(ej.loc.clone(), Import::All { path: scope_j }))
                                            }
                                        }
                                    }
                                },
                                _ => {},
                            }
                        }
                    },
                    _ => {},
                }

                new_node
            }
            Exp::Lambda { id, opt_guard, params, ret } => {
                let scope = self.graph.new_env();
                self.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let unknowns = Prenamer::get_unknowns(FormulaFlag::Val, params, names, scope);

                let mut new_names = names.clone();

                for decl in &unknowns {
                    new_names.push(decl.name());
                    self.graph.declare(scope, decl);
                }

                let child_ctx = PrenameContext {
                    scope,
                    import_scope: Scope::Empty,
                    names: new_names,
                    ..ctx.clone()
                };

                let opt_guard1 = opt_guard.clone().map(|x|
                    match *x {
                        Located { loc, value: e } => Box::new(Located { loc: loc.clone(), value: self.visit_exp(&e, &child_ctx, &loc) })
                    }
                );

                let ret1 = Box::new(Located { loc: ret.loc.clone(), value: self.visit_exp(&ret.value, &child_ctx, &ret.loc) });

                let params_ctx = PrenameContext {
                    unknowns,
                    ..child_ctx.clone()
                };

                let params1 = params.into_iter().map(
                    |Located { loc, value: e }| Located { loc: loc.clone(), value: self.visit_exp(e, &params_ctx, &loc) }
                ).collect();

                Exp::Lambda { id: *id, opt_guard: opt_guard1, params: params1, ret: ret1 }
            },
            Exp::For { id, generator, body } => {
                let scope = self.graph.new_env();
                self.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let unknowns = Prenamer::get_unknowns_from_exp(FormulaFlag::Val, &*generator, names, scope);

                let mut new_names = names.clone();

                for decl in &unknowns {
                    new_names.push(decl.name());
                    self.graph.declare(scope, decl);
                }

                let child_ctx = PrenameContext {
                    scope,
                    import_scope: Scope::Empty,
                    names: new_names,
                    ..ctx.clone()
                };

                let body1 = Box::new(Located { loc: body.loc.clone(), value: self.visit_exp(&body.value, &child_ctx, &body.loc) });

                let params_ctx = PrenameContext {
                    unknowns,
                    ..child_ctx.clone()
                };

                let generator1 = Box::new(Located { loc: generator.loc.clone(), value: self.visit_exp(&generator.value, &params_ctx, &generator.loc) });

                Exp::For { id: *id, generator: generator1, body: body1 }
            },
            Exp::Arrow { id, arg, ret } => {
                let scope = self.graph.new_env();
                self.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let unknowns = Prenamer::get_unknowns_from_exp(FormulaFlag::Val, &*arg, names, scope);

                let mut new_names = names.clone();

                for decl in &unknowns {
                    new_names.push(decl.name());
                    self.graph.declare(scope, decl);
                }

                let child_ctx = PrenameContext {
                    scope,
                    import_scope: Scope::Empty,
                    names: new_names,
                    ..ctx.clone()
                };

                let ret1 = Box::new(Located { loc: ret.loc.clone(), value: self.visit_exp(&ret.value, &child_ctx, &ret.loc) });

                let params_ctx = PrenameContext {
                    unknowns,
                    ..child_ctx.clone()
                };

                let arg1 = Box::new(Located { loc: arg.loc.clone(), value: self.visit_exp(&arg.value, &params_ctx, &arg.loc) });

                Exp::Arrow { id: *id, arg: arg1, ret: ret1 }
            },
            Exp::Within { id, e1, e2 } => {
                let e1_1 = self.visit_exp(&e1.value, &ctx, &e1.loc);
                let e1_2 = Located { loc: e1.loc.clone(), value: e1_1 };

                let scope = self.graph.new_env();
                self.scopes.insert(*id, scope);

                let child_ctx = PrenameContext {
                    scope,
                    import_scope: Scope::Empty,
                    ..ctx.clone()
                };

                let e1_scope = self.lookup_frame(ctx.scope, &e1_2);
                self.graph.import(scope, &Located { loc: e1.loc.clone(), value: Import::All { path: e1_scope } });

                let e2_1 = self.visit_exp(&e2.value, &child_ctx, &e2.loc);

                Exp::Within {
                    id: *id,
                    e1: Box::new(e1_2),
                    e2: Box::new(Located { loc: e2.loc.clone(), value: e2_1 })
                }
            },

            Exp::Name { name, id } => {
                self.scopes.insert(*id, ctx.scope);

                // let _lookup = self.lookup_here(*id, ctx.scope, name.clone());
                self.walk_exp(s, ctx, loc)
            },

            Exp::MixfixApply { es, id } => {
                self.scopes.insert(*id, ctx.scope);

                let new_node = self.walk_exp(s, &ctx, &loc);
                match &new_node {
                    // Exp::MixfixApply { es, id } => {
                    //     let _lookup = self.parse_mixfix(*id, ctx.scope, es);
                    //     new_node
                    // },
                    _ => {
                        new_node
                    },
                }
            },

            _ => {
                self.walk_exp(s, ctx, loc)
            },
        }
    }
}
