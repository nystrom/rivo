use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;
use std::collections::HashMap;

use namer::graph::ScopeGraph;
use namer::graph::EnvIndex;
use namer::symbols::Scope;
use namer::symbols::Env;

use super::loader::Input;

#[derive(Clone, Debug)]
pub enum Bundle {
    Read { source: Source, input: Input },
    Parsed { source: Source, line_map: LineMap, tree: Located<Root> },
    Prenamed { source: Source, line_map: LineMap, tree: Located<Root>, graph: ScopeGraph, scopes: HashMap<NodeId, Scope> },
    Named { source: Source, line_map: LineMap, tree: Located<Root>, graph: ScopeGraph, scopes: HashMap<NodeId, Scope> },
    Core { source: Source, line_map: LineMap, root_scope: Scope }
}

impl Bundle {
    pub fn decode_loc(&self, loc: Loc) -> DecodedLoc {
        match self {
            Bundle::Read { source, .. } => {
                let line_map = LineMap { line_offsets: vec![0] };
                line_map.decode(source.clone(), Loc::no_loc())
            },
            Bundle::Parsed { source, line_map, .. } => {
                line_map.decode(source.clone(), loc)
            },
            Bundle::Prenamed { source, line_map, .. } => {
                line_map.decode(source.clone(), loc)
            },
            Bundle::Named { source, line_map, .. } => {
                line_map.decode(source.clone(), loc)
            },
            Bundle::Core { source, line_map, .. } => {
                line_map.decode(source.clone(), loc)
            },
        }
    }
}