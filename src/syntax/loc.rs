use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug)]
pub struct Loc {
    pub start: Pos,
    pub end: Pos,
    pub source: Option<String>,
}

#[derive(Copy, Clone, Debug)]
pub struct Pos {
    pub offset: u32, // indexed from 0
    pub line: u32,   // indexed from 1
    pub column: u32, // indexed from 1
}

pub const NO_POS: Pos = Pos { offset: 0, line: 1, column: 1 };
pub const NO_LOC: Loc = Loc { start: NO_POS, end: NO_POS, source: None };

#[derive(Clone, Debug)]
pub struct Located<T> {
    pub loc: Loc,
    pub value: T,
}

impl<T> Located<T> {
    pub fn new(t: T, start: Pos, end: Pos, source: String) -> Located<T> {
        Located { value: t, loc: Loc { start: start, end: end, source: Some(source) } }
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
