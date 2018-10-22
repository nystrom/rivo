pub enum Level {
    Bug,
    Fatal,
    Error,
    Info,
    Warning,
    Help
}

use failure::Fail;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "internal error")]
    InternalError,

    #[fail(display = "lexical error")]
    LexError,

    #[fail(display = "syntax error")]
    ParseError,

    #[fail(display = "variable {} not found", name)]
    VarNotFoundError { name: String },

    #[fail(display = "bundle {} not found", name)]
    BundleNotFoundError { name: String },

    #[fail(display = "unresolved mixfix expression")]
    UnresolvedMixfix,

    #[fail(display = "ambigiuous mixfix expression")]
    AmbiguousMixfix,
}
