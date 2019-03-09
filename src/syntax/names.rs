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

    pub fn fresh(prefix: &str) -> Interned {
        let s = format!("{}'{}", prefix, CACHE.lock().unwrap().len());
        Interned::new(&s)
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, Eq, PartialEq, Hash)]
pub enum Name {
    Id(Interned),
    Op(Interned),
    Mixfix(Interned)
}

impl Name {
    pub fn fresh(prefix: &str) -> Name {
        Name::Id(Interned::fresh(prefix))
    }

    pub fn new(s: &str) -> Name {
        Name::Id(Interned::new(s))
    }

    pub fn parts(self) -> Vec<Part> {
        match self {
            Name::Id(x) => vec![Part::Id(x)],
            Name::Op(x) => vec![Part::Op(x)],
            Name::Mixfix(x) => Name::decode_parts(x),
        }
    }

    pub fn encode_parts(parts: &[Part]) -> Interned {
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
                        State::Id => { t.push('@') },
                        State::Op => { t.push('@') },
                    }
                }
                '$' => {
                    match state {
                        State::Init => { state = State::Id; },
                        State::Id => { t.push('$') },
                        State::Op => { t.push('$') },
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
        match self {
            Name::Id(x) => write!(f, "{}", x.to_string()),
            Name::Op(x) => write!(f, "{}", x.to_string()),
            Name::Mixfix(x) => {
                write!(f, "`")?;
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
    }
}

impl Part {
    pub fn to_name(self) -> Name {
        match self {
            Part::Id(x) => Name::Id(x),
            Part::Op(x) => Name::Op(x),
            Part::Placeholder => Name::Op(Interned::new("_"))
        }
    }

    pub fn is_id(self) -> bool {
        match self {
            Part::Id(_) => true,
            _ => false,
        }
    }
    pub fn is_op(self) -> bool {
        match self {
            Part::Op(_) => true,
            _ => false,
        }
    }
    pub fn is_placeholder(self) -> bool {
        match self {
            Part::Placeholder => true,
            _ => false,
        }
    }

}

impl Name {
    pub fn is_bundle_name(self) -> bool {
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

    pub fn is_prefix_name(self) -> bool {
        match self {
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
    pub fn is_brackets_name(self) -> bool {
        match self {
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

    pub fn is_unknown_name(self) -> bool {
        match self {
            Name::Id(x) => {
                if let Some(c) = x.to_string().chars().next() {
                    c.is_lowercase() || c == '_'
                }
                else {
                    false
                }
            },
            _ => false,
        }

    }

    pub fn is_id(self) -> bool {
        match self {
            Name::Id(_) => true,
            _ => false,
        }
    }
    pub fn is_op(self) -> bool {
        match self {
            Name::Op(_) => true,
            _ => false,
        }
    }
    pub fn is_mixfix(self) -> bool {
        match self {
            Name::Mixfix(_) => true,
            _ => false,
        }
    }
}

pub struct FreshNameGenerator {
    prefix: String,
    next: usize,
}

impl FreshNameGenerator {
    pub fn new(prefix: &str) -> FreshNameGenerator {
        FreshNameGenerator {
            prefix: String::from(prefix),
            next: 0
        }
    }

    pub fn fresh(&mut self, prefix: &str) -> Name {
        let x = Interned::new(&format!("{}'{}'{}", prefix, self.prefix, self.next));
        self.next += 1;
        Name::Id(x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::*;

    impl Arbitrary for Part {
        fn arbitrary<G: Gen>(g: &mut G) -> Part {
            if Arbitrary::arbitrary(g) {
                let v = vec!["x", "y", "+", "@", "$", "*", "hello", "ciao"];
                let n: usize = Arbitrary::arbitrary(g);
                let s = v.get(n % v.len()).unwrap();
                if Arbitrary::arbitrary(g) {
                    Part::Id(Interned::new(&s))
                }
                else {
                    Part::Op(Interned::new(&s))
                }
            }
            else {
                Part::Placeholder
            }
        }
    }

    #[allow(unused_attributes)]
    #[quickcheck]
    fn encode_decode(parts: Vec<Part>) -> bool {
        Name::decode_parts(Name::encode_parts(&parts)) == parts
    }
}
