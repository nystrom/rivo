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
