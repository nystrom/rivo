use std::fmt;

use std::sync::Mutex;
use string_interner::{StringInterner, Sym};

// We intern all strings using a static table.
// This table lives forever, but saves us from having to manage lifetimes.
lazy_static! {
    static ref CACHE: Mutex<StringInterner<Sym>> = Mutex::new(StringInterner::default());
}

// Thin wrapper around an interned string.
#[derive(Copy, Clone, Debug, PartialOrd, Ord, Eq, PartialEq, Hash)]
pub struct Interned(Sym);

impl ToString for Interned {
    /// panics if not a valid symbol
    #[inline]
    fn to_string(&self) -> String {
        // lookup in the table
        CACHE.lock().unwrap().resolve(self.0).unwrap().to_string()
    }
}

impl Interned {
    #[inline]
    pub fn new(s: &str) -> Interned {
        Interned(CACHE.lock().unwrap().get_or_intern(s))
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, Eq, PartialEq, Hash)]
pub enum Name {
    Id(Interned),
    Op(Interned),
    Mixfix(Interned)
}

impl Name {
    pub fn parts(&self) -> Vec<Part> {
        match self {
            Name::Id(x) => vec![Part::Id(*x)],
            Name::Op(x) => vec![Part::Op(*x)],
            Name::Mixfix(x) => Name::decode_parts(*x),
        }
    }

    pub fn encode_parts(parts: &Vec<Part>) -> Interned {
        let mut s = String::new();
        for (i, part) in parts.iter().enumerate() {
            if i != 0 {
                s.push_str(" ");
            }
            match part {
                Part::Id(x) => {
                    s.push('$');
                    s.push_str(&x.to_string());
                }
                Part::Op(x) => {
                    s.push('@');
                    s.push_str(&x.to_string());
                }
                Part::Placeholder => {
                    s.push_str("_");
                }
            }
        }
        Interned::new(&s)
    }

    pub fn decode_parts(s: Interned) -> Vec<Part> {
        let mut parts = Vec::new();
        let mut t = String::new();

        enum State { Init, Id, Op };
        let mut state = State::Init;

        for ch in s.to_string().chars() {
            match ch {
                ' ' => {
                    match state {
                        State::Init => {},
                        State::Id => { parts.push(Part::Id(Interned::new(&t))); t.clear(); state = State::Init; },
                        State::Op => { parts.push(Part::Op(Interned::new(&t))); t.clear(); state = State::Init; },
                    }
                }
                '_' => {
                    match state {
                        State::Init => { parts.push(Part::Placeholder); },
                        State::Id => { t.push('_'); },
                        State::Op => { t.push('_'); },
                    }
                }
                '@' => {
                    match state {
                        State::Init => { state = State::Op; },
                        State::Id => {},
                        State::Op => {},
                    }
                }
                '$' => {
                    match state {
                        State::Init => { state = State::Id; },
                        State::Id => {},
                        State::Op => {},
                    }
                }
                ch => {
                    match state {
                        State::Init => { unimplemented!() },
                        State::Id => { t.push(ch); },
                        State::Op => { t.push(ch); },
                    }
                }
            }
        }

        match state {
            State::Init => {},
            State::Id => { parts.push(Part::Id(Interned::new(&t))); t.clear(); },
            State::Op => { parts.push(Part::Op(Interned::new(&t))); t.clear(); },
        }

        parts
    }
}

 /*
// To support copy, we can't store a vector in the
impl Name {
    pub fn parts(&self) -> Vec<Part> {
        match self {
            Name::Id(x) => vec![Part::Id(x)],
            Name::Op(x) => vec![Part::Op(x)],
            Name::Mixfix(x) => { x.split(" ").map(|y| Name::string_to_part(y)).collect() },
        }
    }

    fn to_part(s: &String) -> Part {
        match s {
        }
        }
}
*/


#[derive(Copy, Clone, PartialOrd, Ord, Eq, PartialEq, Hash)]
pub enum Part {
    Id(Interned),
    Op(Interned),
    Placeholder,
}

impl fmt::Debug for Part {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Part::Id(x) => write!(f, "Id(\"{}\")", x.to_string()),
            Part::Op(x) => write!(f, "Op(\"{}\")", x.to_string()),
            Part::Placeholder => write!(f, "Placeholder"),
        }
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Name::Id(x) => write!(f, "Id(\"{}\")", x.to_string()),
            Name::Op(x) => write!(f, "Op(\"{}\")", x.to_string()),
            Name::Mixfix(x) => write!(f, "Mixfix(\"{}\")", x.to_string()),
        }
    }
}

impl fmt::Display for Part {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Part::Id(x) => write!(f, "{}", x.to_string()),
            Part::Op(x) => write!(f, "{}", x.to_string()),
            Part::Placeholder => write!(f, "_"),
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, part) in self.parts().iter().enumerate() {
            if i == 0 {
                write!(f, "{}", part)?;
            }
            else {
                write!(f, " {}", part)?;
            }
        }
        write!(f, "`")
    }
}

impl Part {
    pub fn to_name(&self) -> Name {
        match self {
            Part::Id(x) => Name::Id(*x),
            Part::Op(x) => Name::Op(*x),
            Part::Placeholder => Name::Op(Interned::new("_"))
        }
    }

    pub fn is_id(&self) -> bool {
        match self {
            Part::Id(_) => true,
            _ => false,
        }
    }
    pub fn is_op(&self) -> bool {
        match self {
            Part::Op(_) => true,
            _ => false,
        }
    }
    pub fn is_placeholder(&self) -> bool {
        match self {
            Part::Placeholder => true,
            _ => false,
        }
    }

}

impl Name {
    pub fn is_bundle_name(&self) -> bool {
        match self {
            Name::Id(x) => match x.to_string().chars().next() {
                None => false,
                Some(c) => c.is_uppercase(),
            }
            Name::Op(_) => false,
            Name::Mixfix(_) => false,
            // Name::Mixfix(parts) => {
            //     self.is_prefix_name() && parts.first().unwrap().to_name().is_bundle_name()
            // },
        }
    }

    pub fn is_prefix_name(&self) -> bool {
        match *self {
            Name::Id(_) => false,
            Name::Op(_) => false,
            Name::Mixfix(ref s) => {
                let parts = Name::decode_parts(*s);
                ! parts.is_empty() &&
                parts.first().unwrap().is_id() &&
                parts[1..].iter().all(|p| p.is_placeholder())
            },
        }
    }
    pub fn is_brackets_name(&self) -> bool {
        match *self {
            Name::Id(_) => false,
            Name::Op(_) => false,
            Name::Mixfix(ref s) => {
                let parts = Name::decode_parts(*s);
                ! parts.is_empty() &&
                ! parts.first().unwrap().is_placeholder() &&
                ! parts.last().unwrap().is_placeholder()
            },
        }
    }

    pub fn is_id(&self) -> bool {
        match *self {
            Name::Id(_) => true,
            _ => false,
        }
    }
    pub fn is_op(&self) -> bool {
        match *self {
            Name::Op(_) => true,
            _ => false,
        }
    }
    pub fn is_mixfix(&self) -> bool {
        match *self {
            Name::Mixfix(_) => true,
            _ => false,
        }
    }
}
