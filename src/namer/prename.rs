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

use trace::trace;
trace::init_depth_var!();

#[derive(Clone)]
pub struct PrenameCtx {
    /// The current lexical scope.
    scope: Option<LocalRef>,
    /// Names declared or imported in this scope or a parent scope.
    names: Vec<Name>,
    /// Unknowns declared in this scope.
    unknowns: Vec<Name>,
    /// Are we inside a mixfix expression?
    in_mixfix: bool,
    /// Priority of declarations.
    prio: Prio,
}

impl PrenameCtx {
    pub fn new() -> PrenameCtx {
        PrenameCtx {
            // The initial environment is just a dummy.
            // When we enter the Bundle during the tree traversal,
            // the imports should reference Global. Global is not
            // a parent of the Bundle scope.
            scope: None,
            names: vec![],
            unknowns: vec![],
            in_mixfix: false,
            prio: Prio(0),
        }
    }
}

pub struct Prenamer<'a> {
    // Map from node id to the environment of that node
    pub scopes: &'a mut HashMap<NodeId, LocalRef>,
    pub lookups: &'a mut HashMap<NodeId, LookupIndex>,
    pub mixfixes: &'a mut HashMap<NodeId, MixfixIndex>,
    pub driver: &'a mut driver::Driver,
    pub node_id_generator: &'a mut NodeIdGenerator,
    pub bundle: driver::BundleIndex,
}

struct AddUnknowns<'a> {
    flag: FormulaFlag,
    defined_names: &'a Vec<Name>,
    new_names: &'a mut Vec<Name>,
    graph: &'a mut ScopeGraph,
    scope: LocalRef,
}

impl<'a> AddUnknowns<'a> {
    fn declare_unknowns_in_exps(&mut self, es: &Vec<Located<Exp>>) {
        for e in es {
            self.declare_unknowns_in_exp(e);
        }
    }

    #[cfg_attr(debug_assertions, trace)]
    fn declare_unknown_for_name(&mut self, name: Name, loc: &Loc) {
        if self.defined_names.contains(&name) {
            // already declared in outer scope
            return;
        }
        if self.new_names.contains(&name) {
            // already declared here
            return;
        }

        self.new_names.push(name);

        let decl = match self.flag {
            FormulaFlag::Val => Decl::Val { parent: self.scope, name },
            FormulaFlag::Var => Decl::Var { parent: self.scope, name },
        };

        let r = self.graph.add_env(Located::new(*loc, decl));
        self.graph.declare(self.scope, name, r);
    }

    #[cfg_attr(debug_assertions, trace(disable(ctx)))]
    fn declare_unknowns_in_exp(&mut self, e: &Located<Exp>) {
        match &e.value {
            Exp::Union { box e1, box e2 } => {
                // TODO: we need to verify that all members of the union bind the same unknowns
                // because we don't know which member will match.
                self.declare_unknowns_in_exp(&e1);
                self.declare_unknowns_in_exp(&e2);
            },
            Exp::Intersect { box e1, box e2 } => {
                self.declare_unknowns_in_exp(&e1);
                self.declare_unknowns_in_exp(&e2);
            },
            Exp::Tuple { es } => {
                for e in es {
                    self.declare_unknowns_in_exp(&e);
                }
            },
            Exp::List { es } => {
                for e in es {
                    self.declare_unknowns_in_exp(&e);
                }
            },
            Exp::Where { box pat, box guard } => {
                self.declare_unknowns_in_exp(&pat);
                self.declare_unknowns_in_exp(&guard);
            },
            Exp::Arrow { id, box arg, box ret } => {
                self.declare_unknowns_in_exp(&arg);
                self.declare_unknowns_in_exp(&ret);
            },
            Exp::Assign { box lhs, box rhs } => {
                self.declare_unknowns_in_exp(&lhs);
                self.declare_unknowns_in_exp(&rhs);
                // Names on the left of a binding are also unknowns, regardless of their case.
                match lhs {
                    Located { ref loc, value: Exp::Name { ref name, .. } } => {
                        self.declare_unknown_for_name(*name, &e.loc);
                    },
                    Located { ref loc, value: Exp::Unknown { ref name, .. } } => {
                        self.declare_unknown_for_name(*name, &e.loc);
                    },
                    _ => {},
                }
            },
            Exp::Bind { box lhs, box rhs } => {
                self.declare_unknowns_in_exp(&lhs);
                self.declare_unknowns_in_exp(&rhs);
                // Names on the left of a binding are also unknowns, regardless of their case.
                match lhs {
                    Located { ref loc, value: Exp::Name { ref name, .. } } => {
                        self.declare_unknown_for_name(*name, &e.loc);
                    },
                    Located { ref loc, value: Exp::Unknown { ref name, .. } } => {
                        self.declare_unknown_for_name(*name, &e.loc);
                    },
                    _ => {},
                }
            },
            Exp::Generator { box lhs, box rhs } => {
                self.declare_unknowns_in_exp(&lhs);
                self.declare_unknowns_in_exp(&rhs);
                // Names on the left of a binding are also unknowns, regardless of their case.
                match lhs {
                    Located { ref loc, value: Exp::Name { ref name, .. } } => {
                        self.declare_unknown_for_name(*name, &e.loc);
                    },
                    Located { ref loc, value: Exp::Unknown { ref name, .. } } => {
                        self.declare_unknown_for_name(*name, &e.loc);
                    },
                    _ => {},
                }
            },
            Exp::Select { box exp, name } => {
                match exp {
                    Located { ref loc, value: Exp::Name { .. } } => {},
                    Located { ref loc, value: Exp::Var { .. } } => {},
                    Located { ref loc, value: Exp::MixfixPart { .. } } => {},
                    _ => { self.declare_unknowns_in_exp(&exp); }
                }
            },
            Exp::Within { id, box e1, box e2 } => {
                match e1 {
                    Located { ref loc, value: Exp::Name { .. } } => {},
                    Located { ref loc, value: Exp::Var { .. } } => {},
                    Located { ref loc, value: Exp::MixfixPart { .. } } => {},
                    _ => { self.declare_unknowns_in_exp(&e1); }
                }
                self.declare_unknowns_in_exp(&e2);

            },
            Exp::Apply { box fun, box arg } => {
                self.declare_unknowns_in_exp(&fun);
                self.declare_unknowns_in_exp(&arg);
            },
            Exp::Name { id, name } => {
                self.declare_unknown_for_name(*name, &e.loc);
            },
            Exp::Unknown { id, name } => {
                self.declare_unknown_for_name(*name, &e.loc);
            },
            Exp::Var { id, name } => {},
            Exp::MixfixPart { id, name } => {},
            Exp::MixfixApply { es, id } => {
                for e in es {
                    self.declare_unknowns_in_exp(&e);
                }
            },
            Exp::Layout { id, cmds } => {
                for cmd in cmds {
                    match cmd {
                        Located { loc, value: Cmd::Exp(e) } => {
                            self.declare_unknowns_in_exp(&Located { loc: *loc, value: e.clone() });
                        },
                        _ => {},
                    }

                }
            },
            _ => {},
        }
    }
}

impl<'a> Prenamer<'a> {
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
                Located { loc, value: Def::TraitDef { name, .. } } => {
                    names.extend(Prenamer::get_names(&name).iter().cloned());
                },
                Located { loc, value: Def::FunDef { name, .. } } => {
                    names.extend(Prenamer::get_names(&name).iter().cloned());
                },
                _ => {},
            }
        }

        names
    }

    fn add_imports(&mut self, import_into_scope: LocalRef, parent_scope: LocalRef, cmds: &Vec<Located<Cmd>>) {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: *loc, value: d.clone() }),
                _ => None,
            }
        );
        self.add_imports_from_defs(import_into_scope, parent_scope, defs);
    }

    fn add_root_imports(&mut self, import_into_scope: LocalRef, parent_scope: Ref, cmds: &Vec<Located<Cmd>>) {
        // FIXME cloning!
        let defs = cmds.iter().filter_map(|cmd|
            match cmd {
                Located { loc, value: Cmd::Def(d) } => Some(Located { loc: *loc, value: d.clone() }),
                _ => None,
            }
        );
        self.add_root_imports_from_defs(import_into_scope, parent_scope, defs);
    }

    fn add_imports_from_defs<T>(&mut self, import_into_scope: LocalRef, parent_scope: LocalRef, defs: T)
        where T : Iterator<Item = Located<Def>>
    {
        for def in defs {
            match def {
                Located { loc, value: Def::ImportDef { opt_path, selector } } => {
// FIXME: when importing into a trait body, the parent should include the unknowns of the trait.
// This means we need a block scope as a child of the trait. and we should search the block for members.
                    self.add_import(import_into_scope, parent_scope.to_ref(self.bundle), &opt_path.map(|bx| *bx), &selector, loc);
                },
                _ => {},
            }
        }
    }

    fn add_root_imports_from_defs<T>(&mut self, import_into_scope: LocalRef, parent_scope: Ref, defs: T)
        where T : Iterator<Item = Located<Def>>
    {
        let mut imports_none = false;

        for def in defs {
            match def {
                Located { loc, value: Def::ImportDef { opt_path, selector } } => {
                    imports_none |= selector == Selector::Nothing;
                    self.add_import(import_into_scope, parent_scope, &opt_path.map(|bx| *bx), &selector, loc);
                },
                _ => {},
            }
        }

        // If this is in the root scope and there is no import (), add import Prelude._.
        // FIXME: should we just make the parent of the top-level scope include an import of Prelude?
        // import () will prevent following the parent link.
        if ! imports_none {
            let lookup = self.driver.graph.lookup_inside(Ref::Root, Name::Id(Interned::new("Prelude")));
            let scope = self.driver.graph.get_scope_of_lookup(lookup);
            self.driver.graph.import(import_into_scope, Located { loc: Loc::no_loc(), value: Import::All { path: scope } });
        }
    }

    #[cfg_attr(debug_assertions, trace)]
    fn add_path_import(&mut self, import_into_scope: LocalRef, path: Ref, selector: &Selector, loc: Loc) {
        match selector {
            Selector::All => {
                self.driver.graph.import(import_into_scope, Located { loc, value: Import::All { path } });
            },
            Selector::Nothing => {
                self.driver.graph.import(import_into_scope, Located { loc, value: Import::None { path } });
            },
            Selector::Including { name } => {
                self.driver.graph.import(import_into_scope, Located { loc, value: Import::Including { path, name: *name } });
            },
            Selector::Excluding { name } => {
                self.driver.graph.import(import_into_scope, Located { loc, value: Import::Excluding { path, name: *name } });
            },
            Selector::Renaming { name, rename } => {
                self.driver.graph.import(import_into_scope, Located { loc, value: Import::Renaming { path, name: *name, rename: *rename } });
            },
        }
    }

    #[cfg_attr(debug_assertions, trace)]
    fn add_import(&mut self, import_into_scope: LocalRef, parent_scope: Ref, opt_path: &Option<Located<Exp>>, selector: &Selector, loc: Loc) {
        match opt_path {
            None => {
                self.add_path_import(import_into_scope, parent_scope, selector, loc);
            },
            Some(e) => {
                let inner_scopes = self.lookup_frame_from(import_into_scope, e, false);
                for inner_scope in inner_scopes {
                    self.add_path_import(import_into_scope, inner_scope, selector, loc);
                }
            },
        }
    }

    pub fn lookup_from(&mut self, id: NodeId, scope: LocalRef, name: Name, follow_imports: bool) -> LookupIndex {
        let lookup = self.driver.graph.lookup_from(scope, name, follow_imports);
        self.lookups.insert(id, lookup);
        lookup
    }

    pub fn lookup_inside(&mut self, id: NodeId, scope: Ref, name: Name) -> LookupIndex {
        let lookup = self.driver.graph.lookup_inside(scope, name);
        self.lookups.insert(id, lookup);
        lookup
    }

    pub fn parse_mixfix(&mut self, id: NodeId, scope: LocalRef, es: &Vec<Located<Exp>>, follow_imports: bool) -> MixfixIndex {
        let parts = es.iter().map(|e|
            match e.value {
                Exp::Name { ref name, id } => {
                    let lookup = self.lookup_from(id, scope, *name, follow_imports);
                    MixfixPart { name_ref: Some(lookup) }
                },
                Exp::MixfixPart { ref name, id } => {
                    let lookup = self.lookup_from(id, scope, *name, follow_imports);
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

    pub fn lookup_frame_from(&mut self, scope: LocalRef, e: &Located<Exp>, follow_imports: bool) -> Vec<Ref> {
        match &e.value {
            Exp::Name { name, id } => {
                let lookup = self.lookup_from(*id, scope, *name, follow_imports);
                vec![Ref::Lookup(lookup)]
            },
            Exp::Var { name, id } => {
                let lookup = self.lookup_from(*id, scope, *name, follow_imports);
                vec![Ref::Lookup(lookup)]
            },
            Exp::MixfixApply { es, id } => {
                let lookup = self.parse_mixfix(*id, scope, es, follow_imports);
                vec![Ref::Mixfix(lookup)]
            },
            Exp::Apply { fun, arg } => {
                // When looking up (`List _` Int) it's sufficient to just lookup
                // `List _`. The `Int` will be substituted in at runtime.
                self.lookup_frame_from(scope, &*fun, follow_imports)
            },
            Exp::Within { id, e1, e2 } => {
                let scope = self.scopes.get(&id);
                assert!(scope.is_some());
                scope.iter().map(|local| Ref::Resolved(local.to_global_ref(self.bundle))).collect()
            }
            Exp::Select { exp, name } => {
                let inner_scopes = self.lookup_frame_from(scope, &*exp, follow_imports);
                inner_scopes.iter().map(|inner_scope| {
                    let lookup = self.driver.graph.lookup_inside(*inner_scope, *name);
                    Ref::Lookup(lookup)
                }).collect()
            }
            Exp::Union { box e1, box e2 } => {
                let s1 = self.lookup_frame_from(scope, e1, follow_imports);
                let s2 = self.lookup_frame_from(scope, e2, follow_imports);
                s1.iter().chain(s2.iter()).cloned().collect()
            },
            Exp::Lit { lit } => {
                vec![]
            },
            e => {
                println!("unexpected lookup_frame_from({:?})", &e);
                vec![]
            },
        }
    }

    pub fn lookup_frame_inside(&mut self, scope: Ref, e: &Located<Exp>) -> Vec<Ref> {
        match &e.value {
            Exp::Name { name, id } => {
                let lookup = self.lookup_inside(*id, scope, *name);
                vec![Ref::Lookup(lookup)]
            },
            Exp::Var { name, id } => {
                let lookup = self.lookup_inside(*id, scope, *name);
                vec![Ref::Lookup(lookup)]
            },
            // Exp::MixfixApply { es, id } => {
            //     let lookup = self.parse_mixfix(*id, scope, es);
            //     vec![Ref::Mixfix(lookup)]
            // },
            Exp::Apply { box fun, arg: _ } => {
                // When looking up (`List _` Int) it's sufficient to just lookup
                // `List _`. The `Int` will be substituted in at runtime.
                self.lookup_frame_inside(scope, &fun)
            },
            Exp::Select { exp, name } => {
                let inner_scopes = self.lookup_frame_inside(scope, &*exp);
                inner_scopes.iter().map(|inner_scope| {
                    let lookup = self.driver.graph.lookup_inside(*inner_scope, *name);
                    Ref::Lookup(lookup)
                }).collect()
            }
            Exp::Union { box e1, box e2 } => {
                let s1 = self.lookup_frame_inside(scope, e1);
                let s2 = self.lookup_frame_inside(scope, e2);
                s1.iter().chain(s2.iter()).cloned().collect()
            },
            e => {
                vec![]
            },
        }
    }

    pub fn prio(c: &Located<Cmd>) -> Option<usize> {
        match &c.value {
            Cmd::Def(Def::TraitDef { attrs, .. }) => {
                Prenamer::get_prio_from_attributes(attrs)
            },
            Cmd::Def(Def::FunDef { attrs, .. }) => {
                Prenamer::get_prio_from_attributes(attrs)
            },
            Cmd::Def(Def::FormulaDef { attrs, .. }) => {
                Prenamer::get_prio_from_attributes(attrs)
            },
            _ => None
        }
    }

    fn get_prio_from_attributes(attrs: &Vec<Located<Attr>>) -> Option<usize> {
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
                Located { loc, value: Cmd::Def(Def::FormulaDef { attrs, flag, box formula }) } => {
                    let names = &ctx1.names;
                    let mut add_unknowns = AddUnknowns { 
                        flag: *flag, 
                        defined_names: names, 
                        new_names: &mut new_names, 
                        scope: ctx1.scope.unwrap(), 
                        graph: &mut self.driver.graph
                    };
                    add_unknowns.declare_unknowns_in_exp(&formula);

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
                Located { loc, value: Def::FormulaDef { attrs, flag, box formula } } => {
                    let names = &ctx1.names;
                    let mut add_unknowns = AddUnknowns { 
                        flag: *flag, 
                        defined_names: names, 
                        new_names: &mut new_names, 
                        scope: ctx1.scope.unwrap(), 
                        graph: &mut self.driver.graph 
                    };
                    add_unknowns.declare_unknowns_in_exp(&formula);

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
                // Create the new scope.
                let scope = Decl::new_bundle(self.bundle);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names(cmds);
                let decl_names = Prenamer::get_declared_names(cmds);

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    names: new_names,
                    ..ctx.clone()
                };

                let new_node = Root::Bundle {
                    id: *id,
                    cmds: self.walk_cmd_list(&cmds, &child_ctx)
                };

                // Add imports.
                match new_node {
                    Root::Bundle { ref cmds, .. } => {
                        self.add_root_imports(scope_ref, Ref::Root, cmds);
                    },
                }

                new_node
            },
        }
    }

    fn visit_def(&mut self, s: &'a Def, ctx: &PrenameCtx, loc: &Loc) -> Def {
        match s {
            Def::TraitDef { id, attrs, name, opt_guard, params, supers, defs } => {
                // TODO: this is a bit messy. We should create a block scope containing the unknowns, then create an inner scope for the body, distinct from
                // that. I think we're confusing decls and envs too much here.
                // The problem is the trait scope should be a _child_ of the header, not the parent.

                let parent = ctx.scope.unwrap();
                let param_attrs = params.iter().map(|p| p.attr).collect();

                // Create a block scope for the unknowns.
                let header = Decl::new_block(parent);
                let header_ref = self.driver.graph.add_env(Located::new(*loc, header));

                // Create a scope for the trait itself.
                let scope = Decl::new_trait(header_ref, *name, ctx.prio, param_attrs);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                // Declare in the parent scope.
                // If parent is a Bundle or Block or Trait, we add it to the members.
                self.driver.graph.declare(parent, *name, scope_ref);

                // Declare mixfix parts in the parent scope.
                if let Name::Mixfix(s) = name {
                    let parts = Name::decode_parts(*s);

                    for (i, part) in parts.iter().enumerate() {
                        match part {
                            Part::Id(x) => {
                                let short_name = Name::Id(*x);
                                let mixfix_decl = Located::new(*loc, Decl::MixfixPart { name: short_name, index: i, full: *name, orig: scope_ref });
                                let r = self.driver.graph.add_env(mixfix_decl);
                                self.driver.graph.declare(parent, short_name, r);
                            },
                            Part::Op(x) => {
                                let short_name = Name::Op(*x);
                                let mixfix_decl = Located::new(*loc, Decl::MixfixPart { name: short_name, index: i, full: *name, orig: scope_ref });
                                let r = self.driver.graph.add_env(mixfix_decl);
                                self.driver.graph.declare(parent, short_name, r);
                            },
                            _ => {},
                        }
                    }
                }

                // First visit the input parameters, collecting unknowns there.
                // Then visit the guard.
                // Then visit the body, collecting more unknowns.
                // Then visit the output parameters.

                // Collect the unknowns in the input parameters.
                let mut input_unknowns = Vec::new();

                let mut add_input_unknowns = AddUnknowns {
                    flag: FormulaFlag::Val,
                    defined_names: &ctx.names,
                    scope: header_ref,
                    graph: &mut self.driver.graph,
                    new_names: &mut input_unknowns
                };

                // TODO: we've combined trait and fun defs, but they're really different.
                // Traits defs are always forward mode.

                for Located { loc, value: Param { pat, attr: ParamAttr { mode, .. } } } in params {
                    assert_eq!(*mode, CallingMode::Input);

                    if let CallingMode::Input = mode {
                        add_input_unknowns.declare_unknowns_in_exp(pat);
                    }
                }

                // Unknowns in the guard are also in scope.
                if let Some(guard) = opt_guard {
                    add_input_unknowns.declare_unknowns_in_exp(guard);
                }

                // Add the names of the unknowns to the set of defined names for the child contexts.
                let new_names: Vec<Name> = ctx.names.iter().cloned().chain(input_unknowns.iter().cloned()).collect();

                let input_params_ctx = PrenameCtx {
                    scope: Some(header_ref),
                    names: new_names.clone(),
                    unknowns: input_unknowns,
                    ..ctx.clone()
                };

                let output_params_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    names: new_names.clone(),
                    ..ctx.clone()
                };

                let mut params1 = Vec::new();

                for param in params {
                    match param {
                        Located { loc, value: Param { pat: e, attr: ParamAttr { mode: CallingMode::Input, .. } } } => {
                            let param1 = Located { loc: param.loc, value:
                                Param { pat: box Located { loc: e.loc, value: self.visit_exp(&e.value, &input_params_ctx, &e.loc) }, attr: param.attr } };
                            params1.push(param1);
                        },
                        Located { loc, value: Param { pat: e, attr: ParamAttr { mode: CallingMode::Output, .. } } } => {
                            let param1 = Located { loc: param.loc, value:
                                Param { pat: box Located { loc: e.loc, value: self.visit_exp(&e.value, &output_params_ctx, &e.loc) }, attr: param.attr } };
                            params1.push(param1);
                        },
                    }
                }

                let opt_guard1 = opt_guard.clone().map(|box Located { loc, value: e }|
                    box Located { loc, value: self.visit_exp(&e, &input_params_ctx, &loc) }
                );

                let supers1 = supers.iter().map(|Located { loc, value: e }|
                    Located { loc: *loc, value: self.visit_exp(&e, &output_params_ctx, &loc) }
                ).collect();

                // Declare the supers.
                for e in supers {
                    let super_scopes = self.lookup_frame_from(header_ref, e, true);
                    for super_scope_ref in super_scopes {
                        self.driver.graph.add_super(scope_ref, super_scope_ref);
                    }
                }

                let imported_names = Prenamer::get_imported_names_from_defs(defs.iter().cloned());
                let decl_names = Prenamer::get_declared_names_from_defs(defs.iter().cloned());

                let mut body_names = Vec::new();
                body_names.extend(new_names.iter().cloned());
                body_names.extend(imported_names.iter().cloned());
                body_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    in_mixfix: false,
                    names: body_names,
                    ..ctx.clone()
                };

                // Add imports.
                println!("IMPORT scope = {:?}", scope_ref);
                println!("IMPORT parent = {:?}", header_ref);
                self.add_imports_from_defs(scope_ref, header_ref, defs.iter().cloned());

                let defs1 = defs.iter().map(|def|
                    Located { loc: def.loc, value: self.visit_def(&def.value, &child_ctx, &def.loc) }
                ).collect();

                Def::TraitDef { id: *id, attrs: attrs.clone(), name: *name, opt_guard: opt_guard1, params: params1, supers: supers1, defs: defs1 }
            },
            Def::FunDef { id, attrs, name, opt_guard, opt_body, params, ret } => {
                // Construct the new scope.
                let parent = ctx.scope.unwrap();
                let param_attrs = params.iter().map(|p| p.attr).collect();

                let header = Decl::new_block(parent);
                let header_ref = self.driver.graph.add_env(Located::new(*loc, header));

                let scope = Decl::new_fun(header_ref, *name, ctx.prio, param_attrs, ret.attr);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let block = Decl::new_block(scope_ref);
                let block_ref = self.driver.graph.add_env(Located::new(*loc, block));

                // Declare in the parent scope.
                // If parent is a Bundle or Block or Trait, we add it to the members.
                self.driver.graph.declare(parent, *name, scope_ref);

                // Declare mixfix parts in the parent scope.
                if let Name::Mixfix(s) = name {
                    let parts = Name::decode_parts(*s);

                    for (i, part) in parts.iter().enumerate() {
                        match part {
                            Part::Id(x) => {
                                let short_name = Name::Id(*x);
                                let mixfix_decl = Located::new(*loc, Decl::MixfixPart { name: short_name, index: i, full: *name, orig: scope_ref });
                                let r = self.driver.graph.add_env(mixfix_decl);
                                self.driver.graph.declare(parent, short_name, r);
                            },
                            Part::Op(x) => {
                                let short_name = Name::Op(*x);
                                let mixfix_decl = Located::new(*loc, Decl::MixfixPart { name: short_name, index: i, full: *name, orig: scope_ref });
                                let r = self.driver.graph.add_env(mixfix_decl);
                                self.driver.graph.declare(parent, short_name, r);
                            },
                            _ => {},
                        }
                    }
                }

                // First visit the input parameters, collecting unknowns there.
                // Then visit the guard.
                // Then visit the body, collecting more unknowns.
                // Then visit the output parameters.

                // Collect the unknowns in the input parameters.
                let mut input_unknowns = Vec::new();

                let mut add_input_unknowns = AddUnknowns {
                    flag: FormulaFlag::Val,
                    defined_names: &ctx.names,
                    scope: header_ref,
                    graph: &mut self.driver.graph,
                    new_names: &mut input_unknowns
                };

                // TODO: we've combined trait and fun defs, but they're really different.
                // Traits defs are always forward mode.

                for Located { loc, value: Param { pat, attr: ParamAttr { mode, .. } } } in params {
                    if let CallingMode::Input = mode {
                        add_input_unknowns.declare_unknowns_in_exp(pat);
                    }
                }

                if let Located { loc, value: Param { pat, attr: ParamAttr { mode: CallingMode::Input, .. } } } = ret {
                    add_input_unknowns.declare_unknowns_in_exp(pat);
                }

                // Unknowns in the guard are also in scope.
                if let Some(guard) = opt_guard {
                    add_input_unknowns.declare_unknowns_in_exp(&*guard);
                }

                // Collect the unknowns in the body.
                let mut body_unknowns = Vec::new();
                let new_names = ctx.names.iter().cloned().chain(input_unknowns.iter().cloned()).collect();

                let mut add_body_unknowns = AddUnknowns {
                    flag: FormulaFlag::Val,
                    defined_names: &new_names,
                    scope: block_ref,
                    graph: &mut self.driver.graph,
                    new_names: &mut body_unknowns
                };

                // The body is evaluated before the output parameters are bound.
                // Unknowns in the body are in scope in the output parameters.
                if let Some(body) = opt_body {
                    add_body_unknowns.declare_unknowns_in_exp(&*body);
                }

                // Add the names of the unknowns to the set of defined names for the child contexts.
                let new_input_names: Vec<Name> = ctx.names.iter().cloned().chain(input_unknowns.iter().cloned()).collect();
                let new_names: Vec<Name> = new_input_names.iter().cloned().chain(body_unknowns.iter().cloned()).collect();

                let input_params_ctx = PrenameCtx {
                    scope: Some(header_ref),
                    names: new_input_names.clone(),
                    unknowns: input_unknowns,
                    ..ctx.clone()
                };

                let body_ctx = PrenameCtx {
                    scope: Some(block_ref),
                    names: new_names.clone(),
                    unknowns: body_unknowns,
                    ..ctx.clone()
                };

                let output_params_ctx = PrenameCtx {
                    scope: Some(block_ref),
                    names: new_names,
                    ..ctx.clone()
                };

                // Now, rewrite the children.
                let mut params1 = Vec::new();

                for param in params {
                    match param {
                        Located { loc, value: Param { pat: e, attr: ParamAttr { mode: CallingMode::Input, .. } } } => {
                            let param1 = Located { loc: param.loc, value:
                                Param { pat: box Located { loc: e.loc, value: self.visit_exp(&e.value, &input_params_ctx, &e.loc) }, attr: param.attr } };
                            params1.push(param1);
                        },
                        Located { loc, value: Param { pat: e, attr: ParamAttr { mode: CallingMode::Output, .. } } } => {
                            let param1 = Located { loc: param.loc, value:
                                Param { pat: box Located { loc: e.loc, value: self.visit_exp(&e.value, &output_params_ctx, &e.loc) }, attr: param.attr } };
                            params1.push(param1);
                        },
                    }
                }

                let opt_guard1 = opt_guard.clone().map(|box Located { loc, value: e }|
                    box Located { loc, value: self.visit_exp(&e, &input_params_ctx, &loc) }
                );

                let opt_body1 = opt_body.clone().map(|box Located { loc, value: e }|
                    box Located { loc, value: self.visit_exp(&e, &body_ctx, &loc) }
                );

                let ret1 = match ret {
                    Located { loc, value: Param { pat: e, attr: ParamAttr { mode: CallingMode::Input, .. } } } => {
                        Located { loc: ret.loc, value:
                            Param { pat: box Located { loc: e.loc, value: self.visit_exp(&e.value, &input_params_ctx, &e.loc) }, attr: ret.attr } }
                    },
                    Located { loc, value: Param { pat: e, attr: ParamAttr { mode: CallingMode::Output, .. } } } => {
                        Located { loc: ret.loc, value:
                            Param { pat: box Located { loc: e.loc, value: self.visit_exp(&e.value, &output_params_ctx, &e.loc) }, attr: ret.attr } }
                    },
                };

                Def::FunDef { id: *id, attrs: attrs.clone(), name: *name, opt_guard: opt_guard1, opt_body: opt_body1, params: params1, ret: ret1 }
            },
            Def::FormulaDef { attrs, flag, formula } => {
                let mut unknowns = vec![];

                let mut add_unknowns = AddUnknowns {
                    flag: *flag,
                    defined_names: &ctx.names,
                    scope: ctx.scope.unwrap(),
                    graph: &mut self.driver.graph,
                    new_names: &mut unknowns
                };

                add_unknowns.declare_unknowns_in_exp(&*formula);

                let new_names: Vec<Name> = ctx.names.iter().cloned().chain(unknowns.iter().cloned()).collect();

                let child_ctx = PrenameCtx {
                    unknowns,
                    names: new_names,
                    ..ctx.clone()
                };

                self.walk_def(s, &child_ctx, loc)
            },
            Def::ImportDef { .. } => {
                self.walk_def(s, &ctx, loc)
            },
        }
    }

    #[cfg_attr(debug_assertions, trace(disable(ctx)))]
    fn visit_exp(&mut self, e: &'a Exp, ctx: &PrenameCtx, loc: &Loc) -> Exp {
        match e {
            Exp::Layout { id, cmds } => {
                // Construct the new scope.
                let parent = ctx.scope.unwrap();
                let scope = Decl::new_block(parent);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let names = &ctx.names;
                let imported_names = Prenamer::get_imported_names(cmds);
                let decl_names = Prenamer::get_declared_names(cmds);

                let mut new_names = Vec::new();
                new_names.extend(names.iter().cloned());
                new_names.extend(imported_names.iter().cloned());
                new_names.extend(decl_names.iter().cloned());

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(e, &child_ctx, &loc);

                // Add imports.
                if let Exp::Layout { ref cmds, .. } = new_node {
                    self.add_imports(scope_ref, parent, cmds);
                }

                new_node
            },
            Exp::Lambda { id, opt_guard, params, ret } => {
                let parent = ctx.scope.unwrap();
                let scope = Decl::new_block(parent);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let mut unknowns = Vec::new();

                let mut add_unknowns = AddUnknowns {
                    flag: FormulaFlag::Val,
                    defined_names: &ctx.names,
                    scope: scope_ref,
                    graph: &mut self.driver.graph,
                    new_names: &mut unknowns
                };

                add_unknowns.declare_unknowns_in_exps(params);

                let new_names = ctx.names.iter().chain(unknowns.iter()).cloned().collect();

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let opt_guard1 = opt_guard.clone().map(|box Located { loc, value: e }|
                    box Located { loc, value: self.visit_exp(&e, &child_ctx, &loc) }
                );

                let ret1 = box Located { loc: ret.loc, value: self.visit_exp(&ret.value, &child_ctx, &ret.loc) };

                let params_ctx = PrenameCtx {
                    unknowns,
                    ..child_ctx.clone()
                };

                let params1 = params.into_iter().map(
                    |Located { loc, value: e }| Located { loc: *loc, value: self.visit_exp(e, &params_ctx, &loc) }
                ).collect();

                Exp::Lambda { id: *id, opt_guard: opt_guard1, params: params1, ret: ret1 }
            },
            Exp::For { id, formula, body } => {
                let parent = ctx.scope.unwrap();
                let scope = Decl::new_block(parent);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let names = &ctx.names;

                let mut unknowns = Vec::new();

                let mut add_unknowns = AddUnknowns {
                    flag: FormulaFlag::Val,
                    defined_names: &ctx.names,
                    scope: scope_ref,
                    graph: &mut self.driver.graph,
                    new_names: &mut unknowns
                };

                add_unknowns.declare_unknowns_in_exp(&*formula);

                let new_names = names.iter().chain(unknowns.iter()).cloned().collect();

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let body1 = box Located { loc: body.loc, value: self.visit_exp(&body.value, &child_ctx, &body.loc) };

                let params_ctx = PrenameCtx {
                    unknowns,
                    ..child_ctx.clone()
                };

                let formula1 = box Located { loc: formula.loc, value: self.visit_exp(&formula.value, &params_ctx, &formula.loc) };

                Exp::For { id: *id, formula: formula1, body: body1 }
            },
            Exp::Let { id, formula, body } => {
                let parent = ctx.scope.unwrap();
                let scope = Decl::new_block(parent);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let names = &ctx.names;

                let mut unknowns = Vec::new();

                let mut add_unknowns = AddUnknowns {
                    flag: FormulaFlag::Val,
                    defined_names: &ctx.names,
                    scope: scope_ref,
                    graph: &mut self.driver.graph,
                    new_names: &mut unknowns
                };

                add_unknowns.declare_unknowns_in_exp(&*formula);

                let new_names = names.iter().chain(unknowns.iter()).cloned().collect();

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let body1 = box Located { loc: body.loc, value: self.visit_exp(&body.value, &child_ctx, &body.loc) };

                let params_ctx = PrenameCtx {
                    unknowns,
                    ..child_ctx.clone()
                };

                let formula1 = box Located { loc: formula.loc, value: self.visit_exp(&formula.value, &params_ctx, &formula.loc) };

                Exp::Let { id: *id, formula: formula1, body: body1 }
            },
            Exp::LetVar { id, formula, body } => {
                let parent = ctx.scope.unwrap();
                let scope = Decl::new_block(parent);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let names = &ctx.names;

                let mut unknowns = Vec::new();

                let mut add_unknowns = AddUnknowns {
                    flag: FormulaFlag::Var,
                    defined_names: &ctx.names,
                    scope: scope_ref,
                    graph: &mut self.driver.graph,
                    new_names: &mut unknowns
                };

                add_unknowns.declare_unknowns_in_exp(&*formula);

                let new_names = names.iter().chain(unknowns.iter()).cloned().collect();

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let body1 = box Located { loc: body.loc, value: self.visit_exp(&body.value, &child_ctx, &body.loc) };

                let params_ctx = PrenameCtx {
                    unknowns,
                    ..child_ctx.clone()
                };

                let formula1 = box Located { loc: formula.loc, value: self.visit_exp(&formula.value, &params_ctx, &formula.loc) };

                Exp::LetVar { id: *id, formula: formula1, body: body1 }
            },
            Exp::Arrow { id, arg, ret } => {
                let parent = ctx.scope.unwrap();
                let scope = Decl::new_block(parent);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let names = &ctx.names;

                let mut unknowns = Vec::new();

                let mut add_unknowns = AddUnknowns {
                    flag: FormulaFlag::Val,
                    defined_names: &ctx.names,
                    scope: scope_ref,
                    graph: &mut self.driver.graph,
                    new_names: &mut unknowns
                };

                add_unknowns.declare_unknowns_in_exp(&*arg);

                let new_names = names.iter().cloned().chain(unknowns.iter().cloned()).collect();

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    in_mixfix: false,
                    names: new_names,
                    ..ctx.clone()
                };

                let ret1 = box Located { loc: ret.loc, value: self.visit_exp(&ret.value, &child_ctx, &ret.loc) };

                let params_ctx = PrenameCtx {
                    unknowns,
                    ..child_ctx.clone()
                };

                let arg1 = box Located { loc: arg.loc, value: self.visit_exp(&arg.value, &params_ctx, &arg.loc) };

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

                let parent = ctx.scope.unwrap();
                let scope = Decl::new_block(parent);
                let scope_ref = self.driver.graph.add_env(Located::new(*loc, scope));
                self.scopes.insert(*id, scope_ref);

                let child_ctx = PrenameCtx {
                    scope: Some(scope_ref),
                    in_mixfix: false,
                    ..ctx.clone()
                };

                let e1_scopes = self.lookup_frame_from(parent, &e1_2, true);
                for e1_scope_ref in e1_scopes {
                    self.driver.graph.import(scope_ref, Located { loc: e1.loc, value: Import::All { path: e1_scope_ref } });
                }

                let e2_1 = self.visit_exp(&e2.value, &child_ctx, &e2.loc);

                Exp::Within {
                    id: *id,
                    e1: box e1_2,
                    e2: box Located { loc: e2.loc, value: e2_1 }
                }
            },
            Exp::Name { name, id } => {
                self.scopes.insert(*id, ctx.scope.unwrap());

                let new_node = self.walk_exp(e, &ctx, &loc);

                match &new_node {
                    Exp::Name { name, id } => {
                        let lookup = self.lookup_from(*id, ctx.scope.unwrap(), *name, true);

                        // Record that the name is an unknown.
                        if ctx.unknowns.contains(name) {
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
                self.scopes.insert(*id, ctx.scope.unwrap());

                let child_ctx = PrenameCtx {
                    in_mixfix: true,
                    ..ctx.clone()
                };

                let new_node = self.walk_exp(e, &child_ctx, &loc);

                match &new_node {
                    Exp::MixfixApply { es, id } => {
                        // Lookup, but do no rewrite the tree.
                        // Rewrites are done in Renamer.
                        let lookup = self.parse_mixfix(*id, ctx.scope.unwrap(), es, true);
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
