/// Rather than store node attributes in the tree, we store them
/// in a separate array indexed by NodeId.
/// When rewriting trees, it is important to maintain the NodeId.

use super::trees::NodeId;

/// An example Bundle might look like this.
struct Example {
    // location of the current node
    loc: Attribute<crate::syntax::loc::Loc>,
    // scope of the current node
    scope: Attribute<crate::namer::symbols::Scope>,
    // parent of the current node
    parent: Attribute<crate::syntax::attr::NodeId>,
}

pub struct Attribute<T: Clone> {
    vec: Vec<Option<T>>
}

impl<T: Clone> Attribute<T> {
    pub fn get(&self, id: NodeId) -> Option<T> {
        if id.0 < self.vec.len() {
            self.vec[id.0].clone()
        }
        else {
            None
        }
    }

    pub fn get_or(&self, id: NodeId, t: T) -> T {
        self.get(id).unwrap_or(t)
    }

    pub fn get_or_default(&self, id: NodeId) -> T
        where T: Default
    {
        self.get(id).unwrap_or_default()
    }

    pub fn put(&mut self, id: NodeId, t: T) {
        if id.0 >= self.vec.len() {
            self.vec.resize_default(id.0+1);
        }
        self.vec[id.0] = Some(t)
    }

    pub fn put_if_absent(&mut self, id: NodeId, t: T) {
        match self.get(id) {
            Some(t) => {},
            None => self.put(id, t),
        }
    }
}
