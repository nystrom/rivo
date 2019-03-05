use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;
use std::collections::HashMap;

use namer::graph::ScopeGraph;
use namer::graph::{LookupIndex, MixfixIndex, EnvIndex};
use namer::symbols::LocalRef;

use super::loader::Input;

// TODO: refactor to get rid of the redundant fields.
#[derive(Clone, Debug)]
pub enum Bundle {
    Read { source: Source, input: Input },
    Parsed { source: Source, line_map: LineMap, node_id_generator: NodeIdGenerator, tree: Located<Root> },
    Prenamed { source: Source, line_map: LineMap, node_id_generator: NodeIdGenerator, tree: Located<Root>,
        scopes: HashMap<NodeId, LocalRef>,
        lookups: HashMap<NodeId, LookupIndex>,
        mixfixes: HashMap<NodeId, MixfixIndex>,
    },
    Named { source: Source, line_map: LineMap, node_id_generator: NodeIdGenerator, tree: Located<Root>, scopes: HashMap<NodeId, LocalRef> },
    Core { source: Source, line_map: LineMap, node_id_generator: NodeIdGenerator, root_scope: LocalRef }
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
