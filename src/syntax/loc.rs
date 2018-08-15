use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, PartialEq)]
pub struct Loc {
    pub start: Pos,
    pub end: Pos,
    pub source: Option<String>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pos {
    pub offset: usize, // indexed from 0
    pub line: usize,   // indexed from 1
    pub column: usize, // indexed from 1
}

pub const NO_POS: Pos = Pos { offset: 0, line: 1, column: 1 };
pub const NO_LOC: Loc = Loc { start: NO_POS, end: NO_POS, source: None };

#[derive(Clone, Debug, PartialEq)]
pub struct Located<T> {
    pub loc: Loc,
    pub value: T,
}

impl<T> Located<T> {
    pub fn new(t: T, start: Pos, end: Pos, source: String) -> Located<T> {
        Located { value: t, loc: Loc { start: start, end: end, source: Some(source) } }
    }

    pub fn with_loc(self, loc: &Loc) -> Located<T> {
        Located { loc: loc.clone(), value: self.value }
    }

    pub fn with_value<U>(&self, value: U) -> Located<U> {
        Located { loc: self.loc.clone(), value }
    }

    pub fn map<U, F>(self, f: F) -> Located<U> where F: Fn(T) -> U {
        Located { loc: self.loc.clone(), value: f(self.value) }
    }
    pub fn map_with_loc<U, F>(self, f: F) -> Located<U> where F: Fn(Loc, T) -> U {
        Located { loc: self.loc.clone(), value: f(self.loc.clone(), self.value) }
    }
}

/// Can get a reference to the tree by dereferencing (`*located`)
impl<T> Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}
