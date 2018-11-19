//! The Prename pass traverses the AST and builds the scope graph.
//! It also rewrites the AST to annotate nodes with lookup and scope references.

use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;

use namer::symbols::*;
use namer::graph::*;
use driver;

use visit::rewrite::*;

use std::collections::HashMap;

#[cfg(debug_assertions)]
#[allow(non_upper_case_globals)]
static mut depth: usize = 0;

#[derive(Clone)]
pub struct PrenameCtx {
    /// The current lexical scope.
    scope: Scope,
    /// Names declared or imported in this scope or a parent scope.
    names: Vec<Name>,
    /// Unknowns declared in this scope.
    unknowns: Vec<Located<Decl>>,
    /// Are we inside a mixfix expression?
    in_mixfix: bool,
    /// Priority of declarations.
    prio: Prio,
}

impl PrenameCtx {
    pub fn new() -> PrenameCtx {
        PrenameCtx {
            // The initial environment is Empty, not Global.
            // When we enter the Bundle during the tree traversal,
            // the imports should reference Global. Global is not
            // a parent of the Bundle scope.
            scope: Scope::Empty,
            names: vec![],
            unknowns: vec![],
            in_mixfix: false,
            prio: Prio(0)
        }
    }
}

pub struct Prenamer<'a> {
    pub scopes: &'a mut HashMap<NodeId, Scope>,
    pub lookups: &'a mut HashMap<NodeId, LookupIndex>,
    pub mixfixes: &'a mut HashMap<NodeId, MixfixIndex>,
    pub driver: &'a mut driver::Driver,
    pub node_id_generator: &'a mut NodeIdGenerator,
}

impl<'a> Prenamer<'a> {
    fn get_unknowns_from_exps(flag: FormulaFlag, params: &Vec<Located<Exp>>, defined_names: &Vec<Name>, scope: Scope) -> Vec<Located<Decl>> {
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

    #[trace]
    fn add_unknown_for_name(decls: &mut Vec<Located<Decl>>, flag: FormulaFlag, name: Name, defined_names: &Vec<Name>, scope: Scope, loc: &Loc) -> Vec<Located<Decl>> {
        for decl in decls.iter() {
            if decl.name() == name {
                // already declared here
                return vec![];
            }
        }
        for defined in defined_names {
            if *defined == name {
                // already declared in outer scope
                return vec![];
            }
        }

        let decl = match flag {
            FormulaFlag::Val => Decl::Val { scope, name },
            FormulaFlag::Var => Decl::Var { scope, name },
        };

        decls.push(Located { loc: *loc, value: decl });

        decls.clone()
    }

#[trace]
    fn add_unknowns_for_exp(decls: &mut Vec<Located<Decl>>, flag: FormulaFlag, e: &Located<Exp>, defined_names: &Vec<Name>, scope: Scope) {
        match &e.value {
            Exp::Union { es } => {
                // TODO: we need to verify that all members of the union bind the same unknowns
                // because we don't know which member will match.
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
                // x: t -- t is evaluated in forward mode so should not have unknowns
                // Prenamer::add_unknowns_for_exp(decls, flag, &pat, defined_names, scope);
            },
            Exp::Arrow { id, arg, ret } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &arg, defined_names, scope);
                Prenamer::add_unknowns_for_exp(decls, flag, &ret, defined_names, scope);
            },
            Exp::Assign { lhs, rhs } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &lhs, defined_names, scope);
                Prenamer::add_unknowns_for_exp(decls, flag, &rhs, defined_names, scope);
                // Names on the left of a binding are also unknowns, regardless of their case.
                match **lhs {
                    Located { ref loc, value: Exp::Name { ref name, .. } } => {
                        Prenamer::add_unknown_for_name(decls, flag, *name, defined_names, scope, &e.loc);
                    },
                    Located { ref loc, value: Exp::Unknown { ref name, .. } } => {
                        Prenamer::add_unknown_for_name(decls, flag, *name, defined_names, scope, &e.loc);
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
                        Prenamer::add_unknown_for_name(decls, flag, *name, defined_names, scope, &e.loc);
                    },
                    Located { ref loc, value: Exp::Unknown { ref name, .. } } => {
                        Prenamer::add_unknown_for_name(decls, flag, *name, defined_names, scope, &e.loc);
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
                        Prenamer::add_unknown_for_name(decls, flag, *name, defined_names, scope, &e.loc);
                    },
                    Located { ref loc, value: Exp::Unknown { ref name, .. } } => {
                        Prenamer::add_unknown_for_name(decls, flag, *name, defined_names, scope, &e.loc);
                    },
                    _ => {},
                }
            },
            Exp::Select { exp, name } => {
                match **exp {
                    Located { ref loc, value: Exp::Name { .. } } => {},
                    Located { ref loc, value: Exp::Var { .. } } => {},
                    Located { ref loc, value: Exp::MixfixPart { .. } } => {},
                    _ => { Prenamer::add_unknowns_for_exp(decls, flag, &exp, defined_names, scope); }
                }
            },
            Exp::Within { id, e1, e2 } => {
                match **e1 {
                    Located { ref loc, value: Exp::Name { .. } } => {},
                    Located { ref loc, value: Exp::Var { .. } } => {},
                    Located { ref loc, value: Exp::MixfixPart { .. } } => {},
                    _ => { Prenamer::add_unknowns_for_exp(decls, flag, &e1, defined_names, scope); }
                }
            },
            Exp::Apply { fun, arg } => {
                Prenamer::add_unknowns_for_exp(decls, flag, &fun, defined_names, scope);
                Prenamer::add_unknowns_for_exp(decls, flag, &arg, defined_names, scope);
            },
            Exp::Name { id, name } => {
                Prenamer::add_unknown_for_name(decls, flag, *name, defined_names, scope, &e.loc);
            },
            Exp::Unknown { id, name } => {
                Prenamer::add_unknown_for_name(decls, flag, *name, defined_names, scope, &e.loc);
            },
            Exp::Var { id, name } => {},
            Exp::MixfixPart { id, name } => {},
            Exp::MixfixApply { es, id } => {
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
                Located { loc, value: Def::ImportDef { opt_path, selector: Selector::Including { name } } } => {
                    names.extend(Prenamer::get_names(&name).iter().cloned());
                },
                Located { loc, value: Def::ImportDef { opt_path, selector: Selector::Renaming { name, rename } } } => {
                    names.extend(Prenamer::get_names(&rename).iter().cloned());
                },
                _ => {},
            }
        }

        names
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

    fn get_names(name: &Name) -> Vec<Name> {
        let mut names = Vec::new();

        names.push(*name);

        match name {
            Name::Mixfix(s) => {
                let parts = Name::decode_parts(*s);
                for part in parts {
                    match part {
                        Part::Id(x) => { names.push(Name::Id(x)); },
                        Part::Op(x) => { names.push(Name::Op(x)); },
                        _ => {},
                    }
                }
            },
            _ => {},
        }

        names
    }

    fn get_declared_names_from_defs<T>(defs: T) -> Vec<Name>
        where T : IntoIterator<Item = Located<Def>>
    {
        let mut names = Vec::new();
        for def in defs {
            match def {
                Located { loc, value: Def::MixfixDef { name, .. } } => {
                    names.extend(Prenamer::get_names(&name).iter().cloned());
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

    fn add_imports(&mut self, import_into_scope: Scope, parent_scope: Scope, cmds: &Vec<Located<Cmd>>, is_root: bool) {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: *loc, value: d.clone() }),
                _ => None,
            }
        );
        self.add_imports_from_defs(import_into_scope, parent_scope, defs, is_root);
    }

    fn add_imports_from_defs<T>(&mut self, import_into_scope: Scope, parent_scope: Scope, defs: T, is_root: bool)
        where T : Iterator<Item = Located<Def>>
    {
        let mut imports_none = false;

        for def in defs {
            match def {
                Located { loc, value: Def::ImportDef { opt_path, selector } } => {
                    match selector {
                        Selector::Nothing => {
                            imports_none = true;
                        },
                        _ => {},
                    }

                    self.add_import(import_into_scope, import_into_scope.without_imports(), parent_scope, &opt_path.map(|bx| *bx), selector, loc);
                },
                _ => {},
            }
        }

        // If this is in the root scope and there is no import (), add import Prelude._.
        // FIXME: should we just make the parent of the top-level scope include an import of Prelude.
        // import () will prevent following the parent link.
        if is_root && ! imports_none {
            let lookup = self.driver.graph.lookup(Scope::Global, Name::Id(Interned::new("Prelude")));
            let scope = self.driver.graph.get_scope_of_lookup(lookup);
            self.driver.graph.import(import_into_scope, &Located { loc: Loc::no_loc(), value: Import::All { path: scope } });
        }
    }

    #[cfg_attr(debug_assertions, trace)]
    fn add_import(&mut self, import_into_scope: Scope, lookup_scope: Scope, parent_scope: Scope, opt_path: &Option<Located<Exp>>, selector: Selector, loc: Loc) {
        match opt_path {
            None => {
                match selector {
                    Selector::All => {
                        self.driver.graph.import(import_into_scope, &Located { loc, value: Import::All { path: parent_scope } });
                    },
                    Selector::Nothing => {
                        self.driver.graph.import(import_into_scope, &Located { loc, value: Import::None { path: parent_scope } });
                    },
                    Selector::Including { name } => {
                        self.driver.graph.import(import_into_scope, &Located { loc, value: Import::Including { path: parent_scope, name: name } });
                    },
                    Selector::Excluding { name } => {
                        self.driver.graph.import(import_into_scope, &Located { loc, value: Import::Excluding { path: parent_scope, name: name } });
                    },
                    Selector::Renaming { name, rename } => {
                        self.driver.graph.import(import_into_scope, &Located { loc, value: Import::Renaming { path: parent_scope, name: name, rename: rename } });
                    },
                }
            },
            Some(e) => {
                let inner_scope = self.lookup_frame(lookup_scope, e);
                self.add_import(import_into_scope, inner_scope, inner_scope, &None, selector, loc)
            },
        }
    }

    pub fn lookup(&mut self, id: NodeId, scope: Scope, name: Name) -> LookupIndex {
        let lookup = self.driver.graph.lookup(scope, name);
        self.lookups.insert(id, lookup);
        lookup
    }

    pub fn parse_mixfix(&mut self, id: NodeId, scope: Scope, es: &Vec<Located<Exp>>) -> MixfixIndex {
        let parts = es.iter().map(|e|
            match e.value {
                Exp::Name { ref name, id } => {
                    let lookup = self.lookup(id, scope, *name);
                    MixfixPart { name_ref: Some(lookup) }
                },
                Exp::MixfixPart { ref name, id } => {
                    let lookup = self.lookup(id, scope, *name);
                    MixfixPart { name_ref: Some(lookup) }
                },
                _ =>
                    MixfixPart { name_ref: None }
            }
        ).collect();

        let lookup = self.driver.graph.parse_mixfix(parts);
        self.mixfixes.insert(id, lookup);
        lookup
    }

    pub fn lookup_frame(&mut self, scope: Scope, e: &Located<Exp>) -> Scope {
        match &e.value {
            Exp::Name { name, id } => {
                let lookup = self.lookup(*id, scope, *name);
                self.driver.graph.get_scope_of_lookup(lookup)
            },
            Exp::Var { name, id } => {
                let lookup = self.lookup(*id, scope, *name);
                self.driver.graph.get_scope_of_lookup(lookup)
            },
            Exp::MixfixApply { es, id } => {
                let lookup = self.parse_mixfix(*id, scope, es);
                self.driver.graph.get_scope_of_mixfix(lookup)
            },
            Exp::Apply { fun, arg } => {
                // When looking up (`List _` Int) it's sufficient to just lookup
                // `List _`. The `Int` will be substituted in at runtime.
                self.lookup_frame(scope, &*fun)
            },
            Exp::Record { id, .. } => {
                let scope = self.scopes.get(&id);
                assert!(scope.is_some());
                *scope.unwrap_or(&Scope::Empty)
            },
            Exp::Within { id, e1, e2 } => {
                let scope = self.scopes.get(&id);
                assert!(scope.is_some());
                *scope.unwrap_or(&Scope::Empty)
            }
            Exp::Select { exp, name } => {
                let inner_scope = self.lookup_frame(scope, &*exp);
                let id = self.node_id_generator.new_id();  // FIXME: id shouldn't be needed
                let lookup = self.lookup(id, inner_scope, *name);
                self.driver.graph.get_scope_of_lookup(lookup)
            }
            Exp::Union { es } => {
                match es.as_slice() {
                    [] => Scope::Empty,
                    [e] => self.lookup_frame(scope, e),
                    es => {
                        let env = self.driver.graph.new_env();
                        for e in es {
                            let s = self.lookup_frame(scope, e);
                            // self.driver.graph.import(env, &Located::new(e.loc, Import::All { path: s }));
                            self.driver.graph.include(env, s);
                        }
                        env.to_here()
                    },
                }
            },
            e => {
                // unimplemented!("{:?}", e);
                Scope::Empty
            },
        }
    }

    pub fn prio(c: &Located<Cmd>) -> Option<usize> {
        match &c.value {
            Cmd::Def(Def::MixfixDef { attrs, .. }) => {
                Prenamer::get_prio_from_attributes(attrs)
            },
            Cmd::Def(Def::FormulaDef { attrs, .. }) => {
                Prenamer::get_prio_from_attributes(attrs)
            },
            _ => None
        }
    }

    fn get_prio_from_attributes(attrs: &Vec<Located<Attr>>) -> Option<usize> {
        use num::ToPrimitive;

        for attr in attrs {
            match Prenamer::get_prio_from_attribute(attr) {
                Some(n) => { return Some(n); },
                None => {},
            }
        }

        None
    }

    fn get_prio_from_attribute(attr: &Located<Attr>) -> Option<usize> {
        use num::ToPrimitive;

        match &attr.value {
            Attr::Parens(attr) => Prenamer::get_prio_from_attribute(attr),
            Attr::Braces(attr) => Prenamer::get_prio_from_attribute(attr),
            Attr::Brackets(attr) => Prenamer::get_prio_from_attribute(attr),
            Attr::Seq(attrs) => {
                match attrs.as_slice() {
                    [Located { value: Attr::Name { name: Name::Id(x) }, .. },
                     Located { value: Attr::Lit { lit: Lit::Int { value } }, .. }] => {
                         if x == &Interned::new("prio") {
                             value.to_usize()
                         }
                         else {
                             None
                         }
                    },
                    _ => None,
                }
            },
            _ => None,
        }
    }
}

impl<'a> Prenamer<'a> {
    fn walk_cmd_list(&mut self, body: &Vec<Located<Cmd>>, ctx: &PrenameCtx) -> Vec<Located<Cmd>> {
        let mut result = Vec::new();
        let mut prio = 0;
        let mut new_names = ctx.names.clone();

        for cmd in body {
            // Reset the prio from the attribute, if any.
            if let Some(p) = Prenamer::prio(&cmd) {
                prio = p;
            }

            let ctx1 = PrenameCtx {
                prio: Prio(prio),
                names: new_names.clone(),
                ..ctx.clone()
            };

            match cmd {
                Located { loc, value: Cmd::Def(Def::FormulaDef { attrs, flag, formula }) } => {
                    let names = &ctx1.names;
                    let unknowns = Prenamer::get_unknowns_from_exp(*flag, &*formula, names, ctx1.scope);

                    for decl in &unknowns {
                        new_names.push(decl.name());
                        self.driver.graph.declare(ctx1.scope, decl);
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

    fn walk_def_list(&mut self, body: &Vec<Located<Def>>, ctx: &PrenameCtx) -> Vec<Located<Def>> {
        let mut result = Vec::new();
        let mut prio = 0;
        let mut new_names = ctx.names.clone();

        for def in body {
            let ctx1 = PrenameCtx {
                prio: Prio(prio),
                names: new_names.clone(),
                ..ctx.clone()
            };

            match def {
                Located { loc, value: Def::FormulaDef { attrs, flag, formula } } => {
                    let names = &ctx1.names;
                    let unknowns = Prenamer::get_unknowns_from_exp(*flag, &*formula, names, ctx1.scope);

                    for decl in &unknowns {
                        new_names.push(decl.name());
                        self.driver.graph.declare(ctx1.scope, decl);
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
impl<'tables, 'a> Rewriter<'a, PrenameCtx> for Prenamer<'tables> {
    fn visit_root(&mut self, s: &'a Root, ctx: &PrenameCtx, loc: &Loc) -> Root {
        match s {
            Root::Bundle { id, cmds } => {
                let scope = self.driver.graph.new_env();
                self.driver.graph.set_parent(scope, Scope::Global);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names(cmds);
                let decl_names = Prenamer::get_declared_names(cmds);

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameCtx {
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
                                        self.driver.graph.include(scope.to_here(), *inner_scope);
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
                        self.add_imports(scope, Scope::Global, cmds, true);
                    },
                }

                new_node
            },
        }
    }

    fn visit_def(&mut self, s: &'a Def, ctx: &PrenameCtx, loc: &Loc) -> Def {
        match s {
            Def::MixfixDef { id, attrs, flag, name, opt_guard, params, ret } => {
                let scope = self.driver.graph.new_env();
                self.scopes.insert(*id, scope);

                self.driver.graph.set_parent(scope, ctx.scope);

                // Collect the unknowns in the input parameters.
                let mut unknowns = Vec::new();

                for Located { loc, value: Param { pat, mode, .. } } in params {
                    if let CallingMode::Input = mode {
                        Prenamer::add_unknowns_for_exp(&mut unknowns, FormulaFlag::Val, pat, &ctx.names, scope);
                    }
                }

                if let Located { loc, value: Param { pat, mode: CallingMode::Input, .. } } = ret {
                    Prenamer::add_unknowns_for_exp(&mut unknowns, FormulaFlag::Val, pat, &ctx.names, scope);
                }

                if let Some(guard) = opt_guard {
                    Prenamer::add_unknowns_for_exp(&mut unknowns, FormulaFlag::Val, &*guard, &ctx.names, scope);
                }

                // Declare the unknowns in the new scope here.
                // Add the names of the unknowns to the child scope.
                let mut new_names = ctx.names.clone();

                for decl in &unknowns {
                    new_names.push(decl.name());
                    self.driver.graph.declare(scope, decl);
                }

                let child_ctx = PrenameCtx {
                    scope,
                    names: new_names.clone(),
                    ..ctx.clone()
                };

                let input_params_ctx = PrenameCtx {
                    unknowns,
                    ..child_ctx.clone()
                };

                // The output parameters are logically a child of the guard scope.
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
                    if let Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Output } } = param {
                        inner_frames.push(self.lookup_frame(scope, e));
                    }
                }

                if let Located { loc, value: Param { pat: e, assoc, by_name, mode: CallingMode::Output } } = ret {
                    inner_frames.push(self.lookup_frame(scope, e));
                }

                let decl = match flag {
                    MixfixFlag::Fun => {
                        Located {
                            loc: *loc,
                            value: Decl::Fun {
                                scope: ctx.scope,
                                name: *name,
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
                                name: *name,
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

                self.driver.graph.declare(ctx.scope, &decl);

                // TODO: declare mixfix parts
                if let Name::Mixfix(s) = name {
                    let parts = Name::decode_parts(*s);

                    for (i, part) in parts.iter().enumerate() {
                        match part {
                            Part::Id(x) => {
                                let mixfix_decl = Located { loc: *loc, value: Decl::MixfixPart { name: Name::Id(*x), index: i, full: *name, orig: Box::new(decl.value.clone()) } };
                                self.driver.graph.declare(ctx.scope, &mixfix_decl);
                            },
                            Part::Op(x) => {
                                let mixfix_decl = Located { loc: *loc, value: Decl::MixfixPart { name: Name::Op(*x), index: i, full: *name, orig: Box::new(decl.value.clone()) } };
                                self.driver.graph.declare(ctx.scope, &mixfix_decl);
                            },
                            _ => {},
                        }
                    }
                }

                Def::MixfixDef { id: *id, attrs: attrs.clone(), flag: *flag, name: *name, opt_guard: opt_guard1, params: params1, ret: ret1 }
            },
            Def::FormulaDef { .. } => {
                self.walk_def(s, ctx, loc)
            },
            Def::ImportDef { .. } => {
                // Swap the environment with the weaker import env.
                // This prevents import resolution from searching the frame with
                // the imports themselves.
                let new_scope = ctx.scope.without_imports();

                let child_ctx = PrenameCtx {
                    scope: new_scope,
                    ..ctx.clone()
                };

                self.walk_def(s, &child_ctx, loc)
            },
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(ctx)))]
    fn visit_exp(&mut self, e: &'a Exp, ctx: &PrenameCtx, loc: &Loc) -> Exp {
        match e {
            Exp::Layout { id, cmds } => {
                let scope = self.driver.graph.new_env();
                self.driver.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names(cmds);
                let decl_names = Prenamer::get_declared_names(cmds);

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameCtx {
                    scope,
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(e, &child_ctx, &loc);

                // Include nested records in the scope.
                if let Exp::Layout { ref cmds, .. } = new_node {
                    for cmd in cmds {
                        if let Located { loc, value: Cmd::Exp(Exp::Record { id, .. }) } = cmd {
                            if let Some(inner_scope) = self.scopes.get(&id) {
                                self.driver.graph.include(scope.to_here(), *inner_scope);
                            }
                        }
                    }
                }

                // Add imports.
                if let Exp::Layout { ref cmds, .. } = new_node {
                    self.add_imports(scope, ctx.scope, cmds, false);
                }

                new_node
            },
            Exp::Record { id, defs } => {
                let scope = self.driver.graph.new_env();
                self.driver.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names_from_defs(defs.iter().cloned());
                let decl_names = Prenamer::get_declared_names_from_defs(defs.iter().cloned());

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameCtx {
                    scope,
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(e, &child_ctx, &loc);

                // Add imports.
                if let Exp::Record { ref defs, .. } = new_node {
                    self.add_imports_from_defs(scope, ctx.scope, defs.iter().cloned(), false);
                }

                new_node
            },
            Exp::Union { .. } => {
                let scope = ctx.scope;
                let new_node = self.walk_exp(e, &ctx, &loc);

                // Import the other frames into the records in the union.
                match &new_node {
                    Exp::Union { es } => {
                        for (i, ei) in es.iter().enumerate() {
                            match ei.value {
                                Exp::Record { id, .. } => {
                                    if let Some(s) = self.scopes.get(&id) {
                                        let scope_i = *s; // copy out so we stop borrowing self.
                                        for (j, ej) in es.iter().enumerate() {
                                            if i != j {
                                                let scope_j = self.lookup_frame(scope, ej);
                                                self.driver.graph.import(scope_i, &Located::new(ej.loc, Import::All { path: scope_j }))
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
                let scope = self.driver.graph.new_env();
                self.driver.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let unknowns = Prenamer::get_unknowns_from_exps(FormulaFlag::Val, params, names, scope);

                let mut new_names = names.clone();

                for decl in &unknowns {
                    new_names.push(decl.name());
                    self.driver.graph.declare(scope, decl);
                }

                let child_ctx = PrenameCtx {
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

                let params_ctx = PrenameCtx {
                    unknowns,
                    ..child_ctx.clone()
                };

                let params1 = params.into_iter().map(
                    |Located { loc, value: e }| Located { loc: *loc, value: self.visit_exp(e, &params_ctx, &loc) }
                ).collect();

                Exp::Lambda { id: *id, opt_guard: opt_guard1, params: params1, ret: ret1 }
            },
            Exp::For { id, generator, body } => {
                let scope = self.driver.graph.new_env();
                self.driver.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let unknowns = Prenamer::get_unknowns_from_exp(FormulaFlag::Val, &*generator, names, scope);

                let mut new_names = names.clone();

                for decl in &unknowns {
                    new_names.push(decl.name());
                    self.driver.graph.declare(scope, decl);
                }

                let child_ctx = PrenameCtx {
                    scope,
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let body1 = Box::new(Located { loc: body.loc, value: self.visit_exp(&body.value, &child_ctx, &body.loc) });

                let params_ctx = PrenameCtx {
                    unknowns,
                    ..child_ctx.clone()
                };

                let generator1 = Box::new(Located { loc: generator.loc, value: self.visit_exp(&generator.value, &params_ctx, &generator.loc) });

                Exp::For { id: *id, generator: generator1, body: body1 }
            },
            Exp::Arrow { id, arg, ret } => {
                let scope = self.driver.graph.new_env();
                self.driver.graph.set_parent(scope, ctx.scope);
                self.scopes.insert(*id, scope);

                let names = &ctx.names;
                let unknowns = Prenamer::get_unknowns_from_exp(FormulaFlag::Val, &*arg, names, scope);

                let mut new_names = names.clone();

                for decl in &unknowns {
                    new_names.push(decl.name());
                    self.driver.graph.declare(scope, decl);
                }

                let child_ctx = PrenameCtx {
                    scope,
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let ret1 = Box::new(Located { loc: ret.loc, value: self.visit_exp(&ret.value, &child_ctx, &ret.loc) });

                let params_ctx = PrenameCtx {
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

                let scope = self.driver.graph.new_child_scope(ctx.scope);
                self.scopes.insert(*id, scope);

                let child_ctx = PrenameCtx {
                    scope,
                    in_mixfix: false,
                    ..ctx.clone()
                };

                let e1_scope = self.lookup_frame(ctx.scope, &e1_2);
                self.driver.graph.import(scope, &Located { loc: e1.loc, value: Import::All { path: e1_scope } });

                let e2_1 = self.visit_exp(&e2.value, &child_ctx, &e2.loc);

                Exp::Within {
                    id: *id,
                    e1: Box::new(e1_2),
                    e2: Box::new(Located { loc: e2.loc, value: e2_1 })
                }
            },
            Exp::Name { name, id } => {
                self.scopes.insert(*id, ctx.scope);

                let new_node = self.walk_exp(e, &ctx, &loc);

                match &new_node {
                    Exp::Name { name, id } => {
                        let lookup = self.lookup(*id, ctx.scope, *name);

                        let mut is_unknown = false;

                        // Record that the name is an unknown.
                        for unk in &ctx.unknowns {
                            if unk.value.name() == *name {
                                self.driver.graph.resolve(&lookup, &unk);
                                is_unknown = true;
                            }
                        }

                        if is_unknown {
                            Exp::Unknown { name: *name, id: *id }
                        }
                        else {
                            Exp::Name { name: *name, id: *id }
                        }
                    },
                    _ => {
                        new_node
                    }
                }
            },
            Exp::MixfixApply { es, id } => {
                self.scopes.insert(*id, ctx.scope);

                let child_ctx = PrenameCtx {
                    in_mixfix: true,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(e, &child_ctx, &loc);

                match &new_node {
                    Exp::MixfixApply { es, id } => {
                        // Lookup, but do no rewrite the tree.
                        // Rewrites are done in Renamer.
                        let lookup = self.parse_mixfix(*id, ctx.scope, es);
                        Exp::MixfixApply { es: es.clone(), id: *id }
                    },
                    _ => {
                        new_node
                    },
                }
            },

            _ => {
                self.walk_exp(e, ctx, loc)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syntax::trees::*;
    use syntax::loc::*;

    #[test]
    fn test_name_is_unknown() {
        let a = Name::Id(Interned::new("a"));

        let us = Prenamer::get_unknowns_from_exp(
            FormulaFlag::Val,
            &Located::new(
                Loc::new(1, 1),
                Exp::Name { id: NodeId(0), name: a.clone() }),
            &vec![],
            Scope::Empty);

        let es = vec![Located::new(Loc::new(1, 1), Decl::Val { scope: Scope::Empty, name: a.clone() })];

        assert_eq!(us, es);
    }

    #[test]
    fn test_declared_name_is_not_unknown() {
        let a = Name::Id(Interned::new("a"));

        let us = Prenamer::get_unknowns_from_exp(
            FormulaFlag::Val,
            &Located::new(
                Loc::new(1, 1),
                Exp::Name { id: NodeId(0), name: a.clone() }),
            &vec![a.clone()],
            Scope::Empty);

        let es = vec![];

        assert_eq!(us, es);
    }

    #[test]
    fn test_names_in_mixfix_are_unknown() {
        let a = Name::Id(Interned::new("a"));
        let b = Name::Id(Interned::new("b"));

        let us = Prenamer::get_unknowns_from_exp(
            FormulaFlag::Val,
            &Located::new(
                Loc::new(0, 0),
                Exp::MixfixApply {
                    id: NodeId(0),
                    es: vec![
                        Located::new(Loc::new(1, 1), Exp::Name { id: NodeId(1), name: a.clone() }),
                        Located::new(Loc::new(2, 2), Exp::Lit { lit: Lit::Nothing }),
                        Located::new(Loc::new(3, 3), Exp::Name { id: NodeId(2), name: b.clone() })
                    ]
                }),
            &vec![],
            Scope::Empty);

        let es = vec![
            Located::new(Loc::new(1, 1), Decl::Val { scope: Scope::Empty, name: a.clone() }),
            Located::new(Loc::new(3, 3), Decl::Val { scope: Scope::Empty, name: b.clone() }),
        ];

        assert_eq!(us, es);
    }

    #[test]
    fn test_names_in_mixfix_are_unknown_multiple_occurrences() {
        let a = Name::Id(Interned::new("a"));
        let b = Name::Id(Interned::new("b"));

        let us = Prenamer::get_unknowns_from_exp(
            FormulaFlag::Val,
            &Located::new(
                Loc::new(0, 0),
                Exp::MixfixApply {
                    id: NodeId(0),
                    es: vec![
                        Located::new(Loc::new(1, 1), Exp::Name { id: NodeId(1), name: a.clone() }),
                        Located::new(Loc::new(2, 2), Exp::Lit { lit: Lit::Nothing }),
                        Located::new(Loc::new(3, 3), Exp::Name { id: NodeId(2), name: b.clone() }),
                        Located::new(Loc::new(4, 4), Exp::Name { id: NodeId(2), name: b.clone() }),
                    ] }),
            &vec![],
            Scope::Empty);

        let es = vec![
            Located::new(Loc::new(1, 1), Decl::Val { scope: Scope::Empty, name: a.clone() }),
            Located::new(Loc::new(3, 3), Decl::Val { scope: Scope::Empty, name: b.clone() }),
        ];

        assert_eq!(us, es);
    }

}
