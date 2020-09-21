use thiserror::Error as ThisError;

#[derive(ThisError, Debug)]
pub enum Error {
    #[error("Parse error")]
    Parse(nom::error::ErrorKind),
    #[error("Incomplete")]
    Incomplete,
    #[error("Import error")]
    Import(String),
    #[error("Semantic error")]
    Semantic(String),
}

impl<I> From<nom::Err<(I, nom::error::ErrorKind)>> for Error {
    fn from(e: nom::Err<(I, nom::error::ErrorKind)>) -> Self {
        match e {
            nom::Err::Incomplete(_) => Error::Incomplete,
            nom::Err::Error(e) => Error::Parse(e.1),
            nom::Err::Failure(e) => Error::Parse(e.1),
        }
    }
}
