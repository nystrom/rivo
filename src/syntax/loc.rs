use std::ops::{Deref, DerefMut};
use std::fmt;

use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

    pub fn start_loc(self) -> Loc {
        Loc { start: self.start, end: self.start }
    }
    
    pub fn end_loc(self) -> Loc {
        Loc { start: self.end, end: self.end }
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

#[derive(Clone, Debug, PartialEq)]
pub struct LineMap {
    /// *Char* offsets for each line in the source. NOT the byte offset.
    /// The first element of the vector should be 0.
    /// For a given input input[lines_offsets[i]-1] should be the '\n' before line i.
    pub line_offsets: Vec<u32>,
}

impl LineMap {
    pub fn new(input: &str) -> LineMap {
        let mut v = vec![0];
        let mut cr = false;
        for (i, ch) in input.chars().enumerate() {
            match ch {
                '\r' => {
                    cr = true;
                },
                '\n' => {
                    cr = false;
                    v.push((i+1) as u32);
                },
                _ => {
                    if cr {
                        v.push(i as u32);
                        cr = false;
                    }
                }
            }
        }
        LineMap { line_offsets: v }
    }

    fn get_line(&self, offset: u32) -> usize {
        assert_eq!(self.line_offsets[0], 0);
        match self.line_offsets.binary_search(&offset) {
            Ok(n) => n + 1,
            Err(n) => n,
        }
    }

    // Returns the column of the character at the given byte offset on the given line.
    fn get_column(&self, line: usize, offset: u32) -> usize {
        // let line = self.get_line(offset);
        assert!(line >= 1);
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodedLoc {
    pub start: Pos,
    pub end: Pos,
    pub source: Source,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    pub offset: usize, // indexed from 0
    pub line: usize,   // indexed from 1
    pub column: usize, // indexed from 1
}

pub const NO_LOC: Loc = Loc { start: 0, end: 0 };

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
            loc: other.loc,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_line_map_1() {
         let m = LineMap {
            line_offsets: vec![
                0, 26, 27, 74, 135, 173, 174, 214, 224, 225,                 // 10
                293, 313, 314, 372, 394, 413, 414, 450, 489, 490,            // 20
                526, 565, 567, 568, 621, 676, 732, 786, 815, 848,            // 30
                849, 882, 883, 887, 920, 953, 957, 958, 962, 994,            // 40
                1026, 1059, 1092, 1096, 1097, 1152, 1183, 1184, 1217, 1250,  // 50
                1251, 1283, 1315, 1347, 1348, 1352, 1387, 1422, 1426, 1427,  // 60
                1431, 1465, 1499, 1503, 1504, 1508, 1542, 1576, 1610, 1646,  // 70
                1682, 1719, 1755, 1759, 1760, 1779, 1798, 1817, 1819, 1820,  // 80
                1837, 1856, 1857, 1861, 1899, 1937, 1941, 1943, 1944, 1991   // 90
            ]
        };

        assert_eq!(m.get_line(666), 25);
        assert_eq!(m.get_line(1823), 80);
        assert_eq!(m.get_line(1876), 84);
        assert_eq!(m.get_line(1991), 90);
    }

    #[test]
    pub fn test_line_map_2() {
        let input = r#"// Prelude module of Ivo.

// Mixfix expressions are fully parenthesized.
// This is intended to allow the Prelude to continue to work
// while debugging the mixfix parser.

// Import nothing to avoid a self-loop.
import ()

// To clarify the semantics, we write the Prelude without using any
// syntactic sugar.

// Module for converting a primitive number to a Boolean.
trait PrimBoolean = {
  import Boolean._

  fun fromI32 (Prim.i32._0) = False
  fun fromI32 (x: Prim.i32.I32) = True

  fun fromI64 (Prim.i64._0) = False
  fun fromI64 (x: Prim.i64.I64) = True
}

// Trait used to resolve precedences in the Prelude.
// We use ⊤ for all arguments to ensure no functions
// actually match. The function bodies are empty, which
// would cause an error if the function actually ran.
trait ResolvePrecedences = {
  fun ((Prim.⊤)) || {Prim.⊤}

  fun ((Prim.⊤)) && {Prim.⊤}

  {
    fun (Prim.⊤) == (Prim.⊤)
    fun (Prim.⊤) != (Prim.⊤)
  }

  {
    fun (Prim.⊤) < (Prim.⊤)
    fun (Prim.⊤) > (Prim.⊤)
    fun (Prim.⊤) <= (Prim.⊤)
    fun (Prim.⊤) >= (Prim.⊤)
  }

  // builtin for unions and overloaded for collections
  fun (Prim.⊤) in (Prim.⊤)

  fun (Prim.⊤) ++ ((Prim.⊤))
  fun (Prim.⊤) :: ((Prim.⊤))

  fun ((Prim.⊤)) | (Prim.⊤)
  fun ((Prim.⊤)) ^ (Prim.⊤)
  fun ((Prim.⊤)) & (Prim.⊤)

  {
    fun ((Prim.⊤)) << (Prim.⊤)
    fun ((Prim.⊤)) >> (Prim.⊤)
  }

  {
    fun ((Prim.⊤)) + (Prim.⊤)
    fun ((Prim.⊤)) - (Prim.⊤)
  }

  {
    fun ((Prim.⊤)) * (Prim.⊤)
    fun ((Prim.⊤)) / (Prim.⊤)
    fun ((Prim.⊤)) % (Prim.⊤)
    fun ((Prim.⊤)) div (Prim.⊤)
    fun ((Prim.⊤)) mod (Prim.⊤)
    fun ((Prim.⊤)) quot (Prim.⊤)
    fun ((Prim.⊤)) rem (Prim.⊤)
  }

  fun - (Prim.⊤)
  fun ~ (Prim.⊤)
  fun ! (Prim.⊤)
}

trait Eq (a) = {
  import Boolean._

  {
    fun (x: a) == (y: a) = ! (x != y)
    fun (x: a) != (y: a) = ! (x == y)
  }
}

trait Ordering = (Eq Ordering.Ordering) with {
  import Boolean._"#;

        let expected = LineMap {
            line_offsets: vec![
                0, 26, 27, 74, 135, 173, 174, 214, 224, 225,                 // 10
                293, 313, 314, 372, 394, 413, 414, 450, 489, 490,            // 20
                526, 565, 567, 568, 621, 674, 730, 784, 813, 842,            // 30
                843, 872, 873, 877, 906, 935, 939, 940, 944, 972,            // 40
                1000, 1029, 1058, 1062, 1063, 1118, 1145, 1146, 1175, 1204,  // 50
                1205, 1233, 1261, 1289, 1290, 1294, 1325, 1356, 1360, 1361,  // 60
                1365, 1395, 1425, 1429, 1430, 1434, 1464, 1494, 1524, 1556,  // 70
                1588, 1621, 1653, 1657, 1658, 1675, 1692, 1709, 1711, 1712,  // 80
                1729, 1748, 1749, 1753, 1791, 1829, 1833, 1835, 1836, 1883   // 90
            ]
        };

        let m = LineMap::new(&input.to_string());
        assert_eq!(m, expected);
    }

    #[test]
    #[ignore]
    pub fn test_line_map_3() {
        let input = r#"// Prelude module of Ivo.

// Mixfix expressions are fully parenthesized.
// This is intended to allow the Prelude to continue to work
// while debugging the mixfix parser.

// Import nothing to avoid a self-loop.
import ()

// To clarify the semantics, we write the Prelude without using any
// syntactic sugar.

// Module for converting a primitive number to a Boolean.
trait PrimBoolean = {
  import Boolean._

  fun fromI32 (Prim.i32._0) = False
  fun fromI32 (x: Prim.i32.I32) = True

  fun fromI64 (Prim.i64._0) = False
  fun fromI64 (x: Prim.i64.I64) = True
}

// Trait used to resolve precedences in the Prelude.
// We use ⊤ for all arguments to ensure no functions
// actually match. The function bodies are empty, which
// would cause an error if the function actually ran.
trait ResolvePrecedences = {
  fun ((Prim.⊤)) || {Prim.⊤}

  fun ((Prim.⊤)) && {Prim.⊤}

  {
    fun (Prim.⊤) == (Prim.⊤)
    fun (Prim.⊤) != (Prim.⊤)
  }

  {
    fun (Prim.⊤) < (Prim.⊤)
    fun (Prim.⊤) > (Prim.⊤)
    fun (Prim.⊤) <= (Prim.⊤)
    fun (Prim.⊤) >= (Prim.⊤)
  }

  // builtin for unions and overloaded for collections
  fun (Prim.⊤) in (Prim.⊤)

  fun (Prim.⊤) ++ ((Prim.⊤))
  fun (Prim.⊤) :: ((Prim.⊤))

  fun ((Prim.⊤)) | (Prim.⊤)
  fun ((Prim.⊤)) ^ (Prim.⊤)
  fun ((Prim.⊤)) & (Prim.⊤)

  {
    fun ((Prim.⊤)) << (Prim.⊤)
    fun ((Prim.⊤)) >> (Prim.⊤)
  }

  {
    fun ((Prim.⊤)) + (Prim.⊤)
    fun ((Prim.⊤)) - (Prim.⊤)
  }

  {
    fun ((Prim.⊤)) * (Prim.⊤)
    fun ((Prim.⊤)) / (Prim.⊤)
    fun ((Prim.⊤)) % (Prim.⊤)
    fun ((Prim.⊤)) div (Prim.⊤)
    fun ((Prim.⊤)) mod (Prim.⊤)
    fun ((Prim.⊤)) quot (Prim.⊤)
    fun ((Prim.⊤)) rem (Prim.⊤)
  }

  fun - (Prim.⊤)
  fun ~ (Prim.⊤)
  fun ! (Prim.⊤)
}

trait Eq (a) = {
  import Boolean._

  {
    fun (x: a) == (y: a) = ! (x != y)
    fun (x: a) != (y: a) = ! (x == y)
  }
}

trait Ordering = (Eq Ordering.Ordering) with {
  import Boolean._"#;

        let m = LineMap::new(&input.to_string());

        for i in m.line_offsets {
            if i > 0 {
                let idx: usize = (i-1) as usize;
                match input.to_string().get(idx..=idx) {
                    Some(s) => {
                        assert_eq!(s, "\n");
                    },
                    None => {},
                }
            }
        }
    }

    #[test]
    fn test_line_map_4() {
                            // 0 12 34 56 78 90 1 2 34 567
        let m = LineMap::new(&"1\n2\n3\n4\n5\n6\n\n\n9\n10".to_string());
        assert_eq!(m, LineMap { line_offsets: vec![0,2,4,6,8,10,12,13,14,16] });

        assert_eq!(m.get_line(0), 1);
        assert_eq!(m.get_line(1), 1);
        assert_eq!(m.get_line(2), 2);
        assert_eq!(m.get_line(3), 2);
        assert_eq!(m.get_line(4), 3);
        assert_eq!(m.get_line(5), 3);
        assert_eq!(m.get_line(6), 4);
        assert_eq!(m.get_line(7), 4);
        assert_eq!(m.get_line(8), 5);
        assert_eq!(m.get_line(9), 5);
        assert_eq!(m.get_line(10), 6);
        assert_eq!(m.get_line(11), 6);
        assert_eq!(m.get_line(12), 7);
        assert_eq!(m.get_line(13), 8);
        assert_eq!(m.get_line(14), 9);
        assert_eq!(m.get_line(15), 9);
        assert_eq!(m.get_line(16), 10);
        assert_eq!(m.get_line(17), 10);

        assert_eq!(m.get_column(m.get_line(0), 0), 1);
        assert_eq!(m.get_column(m.get_line(1), 1), 2);
        assert_eq!(m.get_column(m.get_line(2), 2), 1);
        assert_eq!(m.get_column(m.get_line(3), 3), 2);
        assert_eq!(m.get_column(m.get_line(4), 4), 1);
        assert_eq!(m.get_column(m.get_line(5), 5), 2);
        assert_eq!(m.get_column(m.get_line(6), 6), 1);
        assert_eq!(m.get_column(m.get_line(7), 7), 2);
        assert_eq!(m.get_column(m.get_line(8), 8), 1);
        assert_eq!(m.get_column(m.get_line(9), 9), 2);
        assert_eq!(m.get_column(m.get_line(10), 10), 1);
        assert_eq!(m.get_column(m.get_line(11), 11), 2);
        assert_eq!(m.get_column(m.get_line(12), 12), 1);
        assert_eq!(m.get_column(m.get_line(13), 13), 1);
        assert_eq!(m.get_column(m.get_line(14), 14), 1);
        assert_eq!(m.get_column(m.get_line(15), 15), 2);
        assert_eq!(m.get_column(m.get_line(16), 16), 1);
        assert_eq!(m.get_column(m.get_line(17), 17), 2);
    }
}
