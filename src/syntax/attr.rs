/// Rather than store node attributes in the tree, we store them
/// in a separate array indexed by NodeId.
/// When rewriting trees, it is important to maintain the NodeId.

use super::trees::NodeId;
use crate::driver::BundleIndex;
use std::collections::HashMap;

// Each node in the tree has a unique node id.
// Each bundle in the compilation has a set of attributes that map from node id to attribute.
// Attribute references are either local or global.

pub struct LocalRef<T> {
    pub id: NodeId,
    _phantom: std::marker::PhantomData<T>
}

pub struct GlobalRef<T> {
    pub local_ref: LocalRef<T>,
    pub bundle: BundleIndex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalNodeId {
    pub bundle: BundleIndex,
    pub node: NodeId,
}

/// An example Bundle might look like this.
struct Example {
    // location of the current node
    loc: DenseAttribute<crate::syntax::loc::Loc>,
    // scope of the current node
    scope: SparseAttribute<crate::namer::symbols::LocalRef>,
    // parent of the current node
    parent: DenseAttribute<crate::syntax::attr::NodeId>,
}

/// An Attribute is a map from a node id to a T.
pub trait Attribute<T: Clone> {
    fn get(&self, id: NodeId) -> Option<T>;
    fn put(&mut self, id: NodeId, t: T);

    fn get_or(&self, id: NodeId, t: T) -> T {
        self.get(id).unwrap_or(t)
    }
    fn get_or_default(&self, id: NodeId) -> T where T: Default {
        self.get(id).unwrap_or_default()
    }

    fn put_if_absent(&mut self, id: NodeId, t: T) {
        match self.get(id) {
            Some(t) => {},
            None => self.put(id, t),
        }
    }
}

pub struct DenseAttribute<T: Clone> {
    vec: Vec<Option<T>>
}

impl<T: Clone> Attribute<T> for DenseAttribute<T> {
    fn get(&self, id: NodeId) -> Option<T> {
        if id.0 < self.vec.len() {
            self.vec[id.0].clone()
        }
        else {
            None
        }
    }

    fn put(&mut self, id: NodeId, t: T) {
        if id.0 >= self.vec.len() {
            self.vec.resize_with(id.0+1, Default::default);
        }
        self.vec[id.0] = Some(t)
    }
}

pub struct SparseAttribute<T: Clone> {
    map: HashMap<NodeId, T>
}

impl<T: Clone> Attribute<T> for SparseAttribute<T> {
    fn get(&self, id: NodeId) -> Option<T> {
        self.map.get(&id).cloned()
    }

    fn put(&mut self, id: NodeId, t: T) {
        self.map.insert(id, t);
    }
}
