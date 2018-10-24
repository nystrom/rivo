use std::ops::{Deref, DerefMut};
use std::fmt;

use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
// start and end offset into file
pub struct Loc {
    start: u32,
    end: u32,
}

impl Loc {
    pub fn new(start: u32, end: u32) -> Loc {
        assert!(start <= end);
        Loc { start, end }
    }

    pub fn no_loc() -> Loc {
        NO_LOC
    }

    pub fn span(start: Loc, end: Loc) -> Loc {
        assert!(start.start <= end.start);
        assert!(start.end <= end.end);
        Loc::new(start.start, end.end)
    }

    pub fn from<T>(t: &Located<T>) -> Loc {
        t.loc
    }

    pub fn span_from<T, U>(start: &Located<T>, end: &Located<U>) -> Loc {
        Loc::span(start.loc, end.loc)
    }
}

#[derive(Clone, Debug)]
// table of offsets for each line in the source
pub struct LineMap {
    pub line_offsets: Vec<u32>,
}

impl LineMap {
    fn get_line(&self, offset: u32) -> usize {
        assert_eq!(self.line_offsets[0], 0);
        match self.line_offsets.binary_search(&offset) {
            Ok(n) => n + 1,
            Err(n) => n,
        }
    }

    fn get_column(&self, line: usize, offset: u32) -> usize {
        let line = self.get_line(offset);
        let line_offset = self.line_offsets[line-1];
        1 + (offset as usize) - (line_offset as usize)
    }

    pub fn encode(&self, loc: DecodedLoc) -> Loc {
        Loc {
            start: loc.start.offset as u32,
            end: loc.end.offset as u32,
        }
    }

    pub fn decode(&self, source: Source, loc: Loc) -> DecodedLoc {
        let start_line = self.get_line(loc.start);
        let start_col = self.get_column(start_line, loc.start);
        let end_line = self.get_line(loc.end);
        let end_col = self.get_column(end_line, loc.end);

        DecodedLoc {
            start: Pos {
                offset: loc.start as usize,
                line: start_line,
                column: start_col,
            },
            end: Pos {
                offset: loc.end as usize,
                line: end_line,
                column: end_col,
            },
            source,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FileId(usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Source {
    StringSource(String),
    FileSource(PathBuf),
    NoSource,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DecodedLoc {
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

pub const NO_LOC: Loc = Loc { start: 0, end: 0 };

#[derive(Clone, Debug, PartialEq)]
pub struct Located<T> {
    pub loc: Loc,
    pub value: T,
}

impl fmt::Display for DecodedLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}-{}", self.source, self.start, self.end)
    }
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Source::FileSource(ref path) => write!(f, "{}", path.file_name().unwrap_or_default().to_string_lossy()),
            Source::StringSource(_) => write!(f, "(input)"),
            Source::NoSource => write!(f, "(unknown)"),
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
        Located { loc, value }
    }

    pub fn new_from(other: Located<T>, value: T) -> Located<T> {
        Located {
            loc: other.loc.clone(),
            value,
        }
    }

    pub fn with_loc(self, loc: Loc) -> Located<T> {
        Located {
            loc,
            value: self.value,
        }
    }

    pub fn with_value<U>(&self, value: U) -> Located<U> {
        Located {
            loc: self.loc,
            value,
        }
    }

    pub fn map<U, F>(self, f: F) -> Located<U>
    where
        F: Fn(T) -> U,
    {
        Located {
            loc: self.loc,
            value: f(self.value),
        }
    }

    pub fn map_with_loc<U, F>(self, f: F) -> Located<U>
    where
        F: Fn(Loc, T) -> U,
    {
        Located {
            loc: self.loc,
            value: f(self.loc, self.value),
        }
    }
}

pub fn loc_of<T>(locs: Vec<Located<T>>) -> Loc {
    let mut loc = NO_LOC;
    for e in locs {
        if loc == NO_LOC {
            loc = e.loc
        } else {
            loc = Loc {
                start: loc.start,
                end: e.loc.end,
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
