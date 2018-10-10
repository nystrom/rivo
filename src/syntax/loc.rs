use std::ops::{Deref, DerefMut};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Source {
    StringSource(String),
    FileSource(String),
    NoSource
}

#[derive(Clone, Debug, PartialEq)]
pub struct Loc {
    pub start: Pos,
    pub end: Pos,
    pub source: Source,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pos {
    pub offset: usize, // indexed from 0
    pub line: usize,   // indexed from 1
    pub column: usize, // indexed from 1
}

pub const NO_POS: Pos = Pos { offset: 0, line: 1, column: 1 };
pub const NO_LOC: Loc = Loc { start: NO_POS, end: NO_POS, source: Source::NoSource };

impl Loc {
    pub fn no_loc() -> Loc { NO_LOC }

    pub fn prelude_loc() -> Loc {
        Loc {
            start: NO_POS,
            end: NO_POS,
            source: Source::FileSource(String::from("Prelude.ivo"))
        }
    }

    pub fn span(start: Loc, end: Loc) -> Loc {
        Loc {
            start: start.start,
            end: end.end,
            source: start.source.clone()
        }
    }

    pub fn from<T>(t: &Located<T>) -> Loc {
        Loc {
            start: t.loc.start,
            end: t.loc.end,
            source: t.loc.source.clone()
        }
    }

    pub fn span_from<T, U>(start: &Located<T>, end: &Located<U>) -> Loc {
        Loc {
            start: start.loc.start,
            end: end.loc.end,
            source: start.loc.source.clone()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Located<T> {
    pub loc: Loc,
    pub value: T,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}-{}", self.source, self.start, self.end)
    }
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Source::FileSource(ref src) =>
                write!(f, "{}", src),
            Source::StringSource(_) =>
                write!(f, "(input)"),
            Source::NoSource =>
                write!(f, "(unknown)"),
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl<T> Located<T> {
    pub fn new(loc: Loc, value: T) -> Located<T> {
        Located {
            loc,
            value
        }
    }

    pub fn new_from(other: Located<T>, value: T) -> Located<T> {
        Located {
            loc: other.loc.clone(),
            value
        }
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

pub fn loc_of<T>(locs: Vec<Located<T>>) -> Loc {
    let mut loc = NO_LOC;
    for e in locs {
        if loc == NO_LOC {
            loc = e.loc
        }
        else {
            loc = Loc {
                start: loc.start,
                end: e.loc.end,
                source: loc.source
            }
        }
    }
    loc
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
