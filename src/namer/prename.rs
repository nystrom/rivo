//! The Prename pass traverses the AST and builds the scope graph.
//! It also rewrites the AST to annotate nodes with lookup and scope references.

use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;

use namer::symbols::*;
use namer::graph::*;
use namer::glr::*;
use driver;

use visit::rewrite::*;

use std::collections::HashMap;

#[cfg(debug_assertions)]
#[allow(non_upper_case_globals)]
static mut depth: u32 = 0;

#[derive(Clone)]
pub struct PrenameContext {
    /// The current lexical scope.
    scope: Scope,
    /// Names declared or imported in this scope or a parent scope.
    names: Vec<Name>,
    /// Unknowns declared in this scope.
    unknowns: Vec<Located<Decl>>,
    /// Are we inside a mixfix expression?
    in_mixfix: bool,
    /// Are we inside an import expression?
    in_import: bool,
    /// When doing a lookup, should we search the parents of the scope?
    search_parent: bool,
    /// Priority of declarations.
    prio: Prio,
}

impl PrenameContext {
    // The initial environment is Empty, not Global.
    // When we enter the Bundle during the tree traversal,
    // the imports should reference Global. Global is not
    // a parent of the Bundle scope.
    pub fn new() -> PrenameContext {
        PrenameContext {
            scope: Scope::Empty,
            names: vec![],
            unknowns: vec![],
            in_mixfix: false,
            in_import: false,
            search_parent: true,
            prio: Prio(0)
        }
    }
}

pub struct Prenamer<'a> {
    pub graph: &'a mut ScopeGraph,
    pub scopes: &'a mut HashMap<NodeId, Scope>,
    pub driver: &'a mut driver::Driver,
}

impl<'a> Prenamer<'a> {
    fn get_unknowns_from_params(flag: FormulaFlag, params: &Vec<Located<Param>>, defined_names: &Vec<Name>, scope: Scope) -> Vec<Located<Decl>> {
        let mut decls = Vec::new();
        for Located { loc, value: Param { pat: e, .. } } in params {
            Prenamer::add_unknowns_for_exp(&mut decls, flag, e, defined_names, scope);
        }
        decls
    }

    fn get_unknowns(flag: FormulaFlag, params: &Vec<Located<Exp>>, defined_names: &Vec<Name>, scope: Scope) -> Vec<Located<Decl>> {
        let mut decls = Vec::new();
        for e in params {
            Prenamer::add_unknowns_for_exp(&mut decls, flag, e, defined_names, scope);
        }
        decls
    }

    fn get_unknowns_from_exp(flag: FormulaFlag, e: &Located<Exp>, defined_names: &Vec<Name>, scope: Scope) -> Vec<Located<Decl>> {
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

        decls.push(Located { loc: *loc, value: decl });
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
                    Located { ref loc, value: Exp::Name { ref name, .. } } => {
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
                    Located { ref loc, value: Exp::Name { ref name, .. } } => {
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
                    Located { ref loc, value: Exp::Name { ref name, .. } } => {
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
            Exp::Name { id, name, lookup: _ } => {
                Prenamer::add_unknown_for_name(decls, flag, name.clone(), defined_names, scope, &e.loc);
            },
            Exp::MixfixApply { es, id, lookup: _ } => {
                for e in es {
                    Prenamer::add_unknowns_for_exp(decls, flag, &e, defined_names, scope);
                }
            },
            _ => {},
        }
    }

    fn get_imported_names(cmds: &Vec<Located<Cmd>>) -> Vec<Name> {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: *loc, value: d.clone() }),
                _ => None,
            }
        );
        Prenamer::get_imported_names_from_defs(defs)
    }

    fn get_imported_names_from_defs<T>(defs: T) -> Vec<Name>
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

    fn add_imported_names(namespace: &Located<Exp>, names: &mut Vec<Name>) {
        match &namespace.value {
            Exp::Name { name, .. } => {
                names.push(name.clone());
            },
            Exp::Select { name, .. } => {
                names.push(name.clone());
            },
            Exp::MixfixApply { es, .. } => {
                for e in es {
                    Prenamer::add_imported_names(e, names);
                }
            },
            Exp::Apply { fun, .. } => {
                Prenamer::add_imported_names(fun, names);
            },
            Exp::Union { es } => {
                for e in es {
                    Prenamer::add_imported_names(e, names);
                }
            },
            Exp::Tuple { es } => {
                for e in es {
                    Prenamer::add_imported_names(e, names);
                }
            },
            Exp::Within { e2, .. } => {
                Prenamer::add_imported_names(e2, names);
            },
            Exp::Arrow { ret, .. } => {
                Prenamer::add_imported_names(ret, names);
            },
            _ => {
                // bad import!
            },
        }
    }

    fn get_declared_names(cmds: &Vec<Located<Cmd>>) -> Vec<Name> {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: *loc, value: d.clone() }),
                _ => None,
            }
        );
        Prenamer::get_declared_names_from_defs(defs)
    }

    fn get_declared_names_from_defs<T>(defs: T) -> Vec<Name>
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

    /// Checks if the given expression is allowed inside an import.
    fn allowed_in_import(import: &Exp) -> bool {
        match import {
            Exp::Name { .. } => true,
            Exp::Select { exp, .. } => Prenamer::is_stable(exp),
            Exp::Within { e1, e2, .. } => Prenamer::is_stable(e1) && Prenamer::allowed_in_import_scope(e2),
            Exp::Lit { lit: Lit::Nothing } => true,
            Exp::Tuple { es } => es.iter().all(|e| Prenamer::allowed_in_import(e)),
            Exp::Union { es } => es.iter().all(|e| Prenamer::allowed_in_import(e)),
            Exp::Arrow { arg, ret, .. } => {
                match ***ret {
                    Exp::Name { .. } => Prenamer::allowed_in_import(arg),
                    Exp::Lit { lit: Lit::Nothing } => Prenamer::allowed_in_import(arg),
                    _ => false,
                }
            },
            _ => false,
        }
    }

    /// Checks if the given expression is allowed inside an import.
    fn allowed_in_import_scope(import: &Exp) -> bool {
        match import {
            Exp::Lit { lit: Lit::Wildcard } => true,
            Exp::MixfixApply { es, .. } => es.iter().all(|e| Prenamer::is_stable(e)),
            Exp::Tuple { es } => es.iter().all(|e| Prenamer::allowed_in_import_scope(e)),
            Exp::Union { es } => es.iter().all(|e| Prenamer::allowed_in_import_scope(e)),
            Exp::Arrow { arg, ret, .. } => {
                match ***ret {
                    Exp::Name { .. } => Prenamer::allowed_in_import_scope(arg),
                    Exp::Lit { lit: Lit::Nothing } => Prenamer::allowed_in_import_scope(arg),
                    _ => false,
                }
            },
            import => Prenamer::allowed_in_import(import),
        }
    }

    /// Checks if the given expression is (potentially) stable; that is, evaluatable at compile time.
    fn is_stable(e: &Exp) -> bool {
        match e {
            Exp::Name { .. } => true,
            Exp::Select { exp, .. } => Prenamer::is_stable(exp),
            Exp::Within { e1, e2, .. } => Prenamer::is_stable(e1) && Prenamer::is_stable(e2),
            Exp::Lit { .. } => true,
            Exp::MixfixApply { es, .. } => es.iter().all(|e| Prenamer::is_stable(e)),
            Exp::Tuple { es } => es.iter().all(|e| Prenamer::is_stable(e)),
            Exp::Union { es } => es.iter().all(|e| Prenamer::is_stable(e)),
            _ => false,
        }
    }

    fn add_imports(&mut self, import_into_scope: Scope, cmds: &Vec<Located<Cmd>>, is_root: bool) {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: *loc, value: d.clone() }),
                _ => None,
            }
        );
        self.add_imports_from_defs(import_into_scope, defs, is_root);
    }

    fn add_imports_from_defs<T>(&mut self, import_into_scope: Scope, defs: T, is_root: bool)
        where T : Iterator<Item = Located<Def>>
    {
        let mut imports_none = false;

        for def in defs {
            match def {
                Located { loc, value: Def::ImportDef { import } } => {
                    match import.value {
                        Exp::Lit { lit: Lit::Nothing } => {
                            imports_none = true;
                        },
                        _ => {},
                    }

                    self.add_import(import_into_scope, import_into_scope, &*import);
                },
                _ => {},
            }
        }

        // If in the root scope and there is no import (), add import Prelude._.
        if is_root && ! imports_none {
            let lookup = self.graph.lookup_here(Scope::Global, Name::Id("Prelude".to_string()));
            let scope = self.graph.get_scope_of_lookup(lookup);
            self.graph.import(import_into_scope, &Located { loc: Loc::no_loc(), value: Import::All { path: scope } });
        }
    }

    #[cfg_attr(debug_assertions, trace)]
    fn add_import(&mut self, import_into_scope: Scope, lookup_scope: Scope, namespace: &Located<Exp>) {
        let loc = namespace.loc;

        match &namespace.value {
            Exp::Lit { lit: Lit::Wildcard } => {
                self.graph.import(import_into_scope, &Located { loc, value: Import::All { path: lookup_scope.clone() } });
            },
            Exp::Lit { lit: Lit::Nothing } => {
                self.graph.import(import_into_scope, &Located { loc, value: Import::None { path: lookup_scope.clone() } });
            },
            Exp::Select { exp, name } => {
                let scope = self.lookup_frame(lookup_scope, true, &*exp);
                self.graph.import(import_into_scope, &Located { loc, value: Import::Including { path: scope, name: name.clone() } });
            },
            Exp::Within { id, e1, e2 } => {
                let inner_scope = self.lookup_frame(lookup_scope, true, &*e1);
                self.add_import(import_into_scope, inner_scope, &*e2);
            },
            Exp::MixfixApply { es, id, lookup: None } => {
                let lookup = self.parse_mixfix(*id, import_into_scope, true, &es);
                let inner_scope = self.graph.get_scope_of_mixfix(lookup);
                self.graph.import(import_into_scope, &Located { loc, value: Import::Here { path: inner_scope } });
            },
            Exp::MixfixApply { es, id, lookup: Some(lookup) } => {
                let inner_scope = self.graph.get_scope_of_mixfix(*lookup);
                self.graph.import(import_into_scope, &Located { loc, value: Import::Here { path: inner_scope } });
            },
            Exp::Name { id: _, name, lookup: _ } => {
                self.graph.import(import_into_scope, &Located { loc, value: Import::Including { path: lookup_scope.clone(), name: name.clone() } });
            },
            Exp::Union { es } => {
                for e in es {
                    self.add_import(import_into_scope, lookup_scope, e);
                }
            },
            Exp::Tuple { es } => {
                for e in es {
                    self.add_import(import_into_scope, lookup_scope, e);
                }
            },
            Exp::Arrow { id, arg, ret } => {
                match &***arg {
                    Exp::Select { exp, name } => {
                        let inner_scope = self.lookup_frame(import_into_scope, true, &*exp);
                        match &***ret {
                            Exp::Name { id: _, name: rename, lookup: _ } => {
                                self.graph.import(import_into_scope, &Located { loc, value: Import::Renaming { path: inner_scope.clone(), name: name.clone(), rename: rename.clone() } });
                            },
                            Exp::Lit { lit: Lit::Nothing } => {
                                self.graph.import(import_into_scope, &Located { loc, value: Import::Excluding { path: inner_scope.clone(), name: name.clone() } });
                            },
                            _ => {
                                // bad import!
                            },
                        }
                    },
                    Exp::Name { name, .. } => {
                        match &***ret {
                            Exp::Name { id: _, name: rename, lookup: _ } => {
                                self.graph.import(import_into_scope, &Located { loc, value: Import::Renaming { path: lookup_scope.clone(), name: name.clone(), rename: rename.clone() } });
                            },
                            Exp::Lit { lit: Lit::Nothing } => {
                                self.graph.import(import_into_scope, &Located { loc, value: Import::Excluding { path: lookup_scope.clone(), name: name.clone() } });
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

    pub fn parse_mixfix(&mut self, id: NodeId, scope: Scope, in_import: bool, es: &Vec<Located<Exp>>) -> MixfixIndex {
        let parts = es.iter().map(|e|
            match e.value {
                Exp::Name { ref name, id, lookup: None } => {
                    if in_import {
                        let lookup = self.graph.lookup_for_import(scope, name.clone());
                        MixfixPart { name_ref: Some(lookup) }
                    }
                    else {
                        let lookup = self.graph.lookup(scope, name.clone());
                        MixfixPart { name_ref: Some(lookup) }
                    }
                },
                Exp::Name { ref name, id, lookup: Some(lookup) } => {
                    MixfixPart { name_ref: Some(lookup) }
                },
                _ =>
                    MixfixPart { name_ref: None }
            }
        ).collect();

        self.graph.parse_mixfix(parts)
    }

    pub fn lookup_frame(&mut self, scope: Scope, in_import: bool, e: &Located<Exp>) -> Scope {
        match &e.value {
            Exp::Name { name, id, lookup: None } => {
                if in_import {
                    let lookup = self.graph.lookup_for_import(scope, name.clone());
                    self.graph.get_scope_of_lookup(lookup)
                }
                else {
                    let lookup = self.graph.lookup(scope, name.clone());
                    self.graph.get_scope_of_lookup(lookup)
                }
            },
            Exp::Name { name, id, lookup: Some(lookup) } => {
                self.graph.get_scope_of_lookup(*lookup)
            },
            Exp::MixfixApply { es, id, lookup: None } => {
                let lookup = self.parse_mixfix(*id, scope, in_import, es);
                self.graph.get_scope_of_mixfix(lookup)
            },
            Exp::MixfixApply { es, id, lookup: Some(lookup) } => {
                self.graph.get_scope_of_mixfix(*lookup)
            },
            Exp::Apply { fun, arg } => {
                // When looking up (`List _` Int) it's sufficient to just lookup
                // `List _`. The `Int` will be substituted in at runtime.
                self.lookup_frame(scope, in_import, &*fun)
            },
            Exp::Record { id, .. } => {
                let scope = self.scopes.get(&id);
                *scope.unwrap_or(&Scope::Empty)
            },
            Exp::Within { id, e1, e2 } => {
                let scope = self.scopes.get(&id);
                *scope.unwrap_or(&Scope::Empty)
            }
            Exp::Union { es } => {
                let env = self.graph.new_env();
                for e in es {
                    let s = self.lookup_frame(scope, in_import, e);
                    self.graph.include(env, s);
                }
                env
            },
            e => {
                // unimplemented!("{:?}", e);
                Scope::Empty
            },
        }
    }
}

impl<'a> Prenamer<'a> {
    fn walk_cmd_list(&mut self, body: &Vec<Located<Cmd>>, ctx: &PrenameContext) -> Vec<Located<Cmd>> {
        let mut result = Vec::new();
        let mut prio = 0;
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
                    result.push(Located { loc: cmd.loc, value: cmd1 });
                },
                _ => {
                    let cmd1 = self.visit_cmd(&cmd.value, &ctx1, &cmd.loc);
                    result.push(Located { loc: cmd.loc, value: cmd1 });
                },
            }

            prio += 1;
        }
        result
    }

    fn walk_def_list(&mut self, body: &Vec<Located<Def>>, ctx: &PrenameContext) -> Vec<Located<Def>> {
        let mut result = Vec::new();
        let mut prio = 0;
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
                    result.push(Located { loc: def.loc, value: def1 });
                },
                _ => {
                    let def1 = self.visit_def(&def.value, &ctx1, &def.loc);
                    result.push(Located { loc: def.loc, value: def1 });
                },
            }

            prio += 1;
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

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names(cmds);
                let decl_names = Prenamer::get_declared_names(cmds);

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameContext {
                    scope,
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
                    },
                }

                // Add imports.
                match new_node {
                    Root::Bundle { ref cmds, .. } => {
                        self.add_imports(scope, cmds, true);
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
                    names: new_names.clone(),
                    ..ctx.clone()
                };

                let input_params_ctx = PrenameContext {
                    unknowns,
                    ..child_ctx.clone()
                };

                let opt_guard1 = opt_guard.clone().map(|x|
                    match *x {
                        Located { loc, value: e } => Box::new(Located { loc, value: self.visit_exp(&e, &child_ctx, &loc) })
                    }
                );

                let ret1 = match ret {
                    Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Input } } => {
                        Located { loc: ret.loc, value:
                            Param { pat: Box::new(Located { loc: e.loc, value: self.visit_exp(&e.value, &input_params_ctx, &e.loc) }), assoc: *assoc, by_name: *by_name, mode: CallingMode::Input } }
                    },
                    Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Output } } => {
                        Located { loc: ret.loc, value:
                            Param { pat: Box::new(Located { loc: e.loc, value: self.visit_exp(&e.value, &child_ctx, &e.loc) }), assoc: *assoc, by_name: *by_name, mode: CallingMode::Output } }
                    },
                };

                let mut params1 = Vec::new();

                for param in params {
                    match param {
                        Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Input } } => {
                            let param1 = Located { loc: ret.loc, value:
                                Param { pat: Box::new(Located { loc: e.loc, value: self.visit_exp(&e.value, &input_params_ctx, &e.loc) }), assoc: *assoc, by_name: *by_name, mode: CallingMode::Input } };
                            params1.push(param1);
                        },
                        Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Output } } => {
                            let param1 = Located { loc: ret.loc, value:
                                Param { pat: Box::new(Located { loc: e.loc, value: self.visit_exp(&e.value, &child_ctx, &e.loc) }), assoc: *assoc, by_name: *by_name, mode: CallingMode::Output } };
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

                let mut inner_frames = vec![];

                for param in params {
                    match param {
                        Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Output } } => {
                            inner_frames.push(self.lookup_frame(ctx.scope, false, e));
                        },
                        _ => {},
                    }
                }

                match ret {
                    Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Output } } => {
                        inner_frames.push(self.lookup_frame(ctx.scope, false, e));
                    },
                    _ => {},
                }

                let decl = match flag {
                    MixfixFlag::Fun => {
                        Located {
                            loc: *loc,
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
                        }
                    },
                    MixfixFlag::Trait => {
                        Located {
                            loc: *loc,
                            value: Decl::Trait {
                                scope: ctx.scope,
                                name: name.clone(),
                                param_assocs,
                                param_convs,
                                param_modes,
                                ret_mode: *ret_mode,
                                ret_conv: *ret_conv,
                                prio,
                                body: inner_frames,
                            }
                        }
                    },
                };

                self.graph.declare(ctx.scope, &decl);

                // TODO: declare mixfix parts
                match name {
                    Name::Mixfix(parts) => {
                        for (i, part) in parts.iter().enumerate() {
                            match part {
                                Part::Id(x) => {
                                    let mixfix_decl = Located { loc: *loc, value: Decl::MixfixPart { name: Name::Id(x.clone()), index: i, full: name.clone(), orig: Box::new(decl.value.clone()) } };
                                    self.graph.declare(ctx.scope, &mixfix_decl);
                                },
                                Part::Op(x) => {
                                    let mixfix_decl = Located { loc: *loc, value: Decl::MixfixPart { name: Name::Op(x.clone()), index: i, full: name.clone(), orig: Box::new(decl.value.clone()) } };
                                    self.graph.declare(ctx.scope, &mixfix_decl);
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
                    in_import: true,
                    ..ctx.clone()
                };

                if ! Prenamer::allowed_in_import(import) {
                    self.driver.error(Located { loc: *loc, value: "Non-stable expression found in import definition.".to_owned() });
                    return s.clone();
                }

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

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names(cmds);
                let decl_names = Prenamer::get_declared_names(cmds);

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameContext {
                    scope,
                    in_import: false,
                    in_mixfix: false,
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

                    },
                    _ => {},
                }

                // Add imports.
                match new_node {
                    Exp::Layout { ref cmds, .. } => {
                        self.add_imports(scope, cmds, false);
                    },
                    _ => {},
                }

                new_node
            },
            Exp::Record { id, defs } => {
                let scope = self.graph.new_env();
                self.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names_from_defs(defs.iter().cloned());
                let decl_names = Prenamer::get_declared_names_from_defs(defs.iter().cloned());

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameContext {
                    scope,
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(s, &child_ctx, &loc);

                // Add imports.
                match new_node {
                    Exp::Record { ref defs, .. } => {
                        self.add_imports_from_defs(scope, defs.iter().cloned(), false);
                    },
                    _ => {},
                }

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
                                                let scope_j = self.lookup_frame(scope, ctx.in_import, ej);
                                                self.graph.import(*scope_i, &Located::new(ej.loc, Import::All { path: scope_j }))
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
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let opt_guard1 = opt_guard.clone().map(|x|
                    match *x {
                        Located { loc, value: e } => Box::new(Located { loc, value: self.visit_exp(&e, &child_ctx, &loc) })
                    }
                );

                let ret1 = Box::new(Located { loc: ret.loc, value: self.visit_exp(&ret.value, &child_ctx, &ret.loc) });

                let params_ctx = PrenameContext {
                    unknowns,
                    ..child_ctx.clone()
                };

                let params1 = params.into_iter().map(
                    |Located { loc, value: e }| Located { loc: *loc, value: self.visit_exp(e, &params_ctx, &loc) }
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
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let body1 = Box::new(Located { loc: body.loc, value: self.visit_exp(&body.value, &child_ctx, &body.loc) });

                let params_ctx = PrenameContext {
                    unknowns,
                    ..child_ctx.clone()
                };

                let generator1 = Box::new(Located { loc: generator.loc, value: self.visit_exp(&generator.value, &params_ctx, &generator.loc) });

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
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let ret1 = Box::new(Located { loc: ret.loc, value: self.visit_exp(&ret.value, &child_ctx, &ret.loc) });

                let params_ctx = PrenameContext {
                    unknowns,
                    ..child_ctx.clone()
                };

                let arg1 = Box::new(Located { loc: arg.loc, value: self.visit_exp(&arg.value, &params_ctx, &arg.loc) });

                Exp::Arrow { id: *id, arg: arg1, ret: ret1 }
            },
            Exp::Within { id, e1, e2 } => {
                // The rules here are a little complicated.
                // e1.e2 is equivalent to { import e1._; e2 }
                // Except that we need check that e2 after naming
                // is an access to a single decl in e1.
                // M.(f 1) --> M.`f _` 1            -- ok
                // M.(1 + 2) --> `_ + _` 1 2        -- error
                // M.(f x) --> M.`f _` M.x          -- error
                // M.(f x) --> `f _` M.x            -- error

                let e1_1 = self.visit_exp(&e1.value, &ctx, &e1.loc);
                let e1_2 = Located { loc: e1.loc, value: e1_1 };

                let scope = self.graph.new_env();
                self.scopes.insert(*id, scope);

                let child_ctx = PrenameContext {
                    scope,
                    in_mixfix: false,
                    ..ctx.clone()
                };

                let e1_scope = self.lookup_frame(ctx.scope, ctx.in_import, &e1_2);
                self.graph.import(scope, &Located { loc: e1.loc, value: Import::All { path: e1_scope } });

                let e2_1 = self.visit_exp(&e2.value, &child_ctx, &e2.loc);

                Exp::Within {
                    id: *id,
                    e1: Box::new(e1_2),
                    e2: Box::new(Located { loc: e2.loc, value: e2_1 })
                }
            },
            Exp::Name { name, id, lookup: None } => {
                self.scopes.insert(*id, ctx.scope);

                let new_node = self.walk_exp(s, &ctx, &loc);

                match &new_node {
                    Exp::Name { name, id, lookup: None } => {
                        let lookup = self.graph.lookup(ctx.scope, name.clone());
                        Exp::Name { name: name.clone(), id: *id, lookup: Some(lookup) }
                    },
                    _ => {
                        new_node
                    }
                }
            },
            Exp::MixfixApply { es, id, lookup: None } => {
                self.scopes.insert(*id, ctx.scope);

                let child_ctx = PrenameContext {
                    in_mixfix: true,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(s, &child_ctx, &loc);

                match &new_node {
                    Exp::MixfixApply { es, id, lookup: None } => {
                        // Lookup, but do no rewrite the tree.
                        // Rewrites are done in Renamer.
                        let lookup = self.parse_mixfix(*id, ctx.scope, ctx.in_import, es);
                        Exp::MixfixApply { es: es.clone(), id: *id, lookup: Some(lookup) }
                    },
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
