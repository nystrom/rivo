use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Name {
    Id(String),
    Op(String),
    Mixfix(Vec<Part>),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Part {
    Id(String),
    Op(String),
    Placeholder,
}


impl fmt::Display for Part {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Part::Id(x) => write!(f, "{}", x),
            Part::Op(x) => write!(f, "{}", x),
            Part::Placeholder => write!(f, "_"),
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Name::Id(x) => write!(f, "{}", x),
            Name::Op(x) => write!(f, "{}", x),
            Name::Mixfix(parts) => {
                write!(f, "`")?;
                for (i, part) in parts.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{}", part)?;
                    }
                    else {
                        write!(f, " {}", part)?;
                    }
                }
                write!(f, "`")
            },
        }
    }
}

impl Part {
    pub fn to_name(&self) -> Name {
        match self {
            Part::Id(x) => Name::Id(x.clone()),
            Part::Op(x) => Name::Op(x.clone()),
            Part::Placeholder => Name::Op("_".to_string())
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
            Name::Id(x) => match x.chars().next() {
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
            Name::Mixfix(ref parts) => {
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
            Name::Mixfix(ref parts) => {
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
