use std::collections::VecDeque;
use std::collections::HashSet;
use std::fmt;
use super::symbols::LookupRef;

// #[derive(Debug)]
pub struct Worklist {
    changed: bool,
    worklist: VecDeque<LookupRef>,
    in_worklist: HashSet<LookupRef>,
}

impl fmt::Debug for Worklist {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.worklist.fmt(f)
    }
}

impl Worklist {
    pub fn new() -> Self {
        Worklist {
            changed: true,
            worklist: VecDeque::new(),
            in_worklist: HashSet::new()
        }
    }

    pub fn push_back(&mut self, r: LookupRef) {
        if self.in_worklist.contains(&r) {
            return;
        }
        self.changed = true;
        self.in_worklist.insert(r);
        self.worklist.push_back(r)
    }

    pub fn pop_front(&mut self) -> Option<LookupRef> {
        if let Some(r) = self.worklist.pop_front() {
            self.in_worklist.remove(&r);
            Some(r)
        }
        else {
            None
        }
    }

    pub fn is_changed(&self) -> bool {
        self.changed
    }

    pub fn reset_changed(&mut self) {
        self.changed = false;
    }

    pub fn is_empty(&self) -> bool {
        self.worklist.is_empty()
    }

    pub fn contains(&self, r: &LookupRef) -> bool {
        self.in_worklist.contains(r)
    }
}
