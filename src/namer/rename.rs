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

impl<'a> Renamer<'a> {
    pub fn lookup_here(&mut self, id: NodeId, scope: Scope, name: Name) -> LookupHereIndex {
            let lookup = self.graph.lookup_here(scope, name);
            lookup
    }

    pub fn lookup(&mut self, id: NodeId, scope: Scope, name: Name) -> LookupIndex {
            let lookup = self.graph.lookup(scope, name);
            lookup
    }

    pub fn parse_mixfix(&mut self, id: NodeId, scope: Scope, es: &Vec<Located<Exp>>) -> MixfixIndex {
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
            lookup
    }
}

pub struct Renamer<'a> {
    pub graph: &'a mut ScopeGraph,
    pub driver: &'a mut driver::Interpreter,
}

// visit any node that has a node id.
impl<'tables, 'a> Rewriter<'a, Scope> for Renamer<'tables> {
    fn visit_exp(&mut self, s: &'a Exp, scope: &Scope, loc: &Loc) -> Exp {
        match s {
            Exp::Name { name, id } => {
                let _lookup = self.lookup_here(*id, *scope, name.clone());
                self.walk_exp(s, ctx, loc)
            },

            Exp::MixfixApply { es, id } => {
                let new_node = self.walk_exp(s, &ctx, &loc);
                match &new_node {
                    Exp::MixfixApply { es, id } => {
                        let _lookup = self.parse_mixfix(*id, *scope, es);
                        new_node
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
