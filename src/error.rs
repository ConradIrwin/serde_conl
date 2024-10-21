use std::fmt::{self, Display};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub lno: usize,
    pub msg: String,
}

impl Error {
    pub fn new(lno: usize, msg: impl Into<String>) -> Self {
        Error {
            lno,
            msg: msg.into(),
        }
    }

    pub(crate) fn set_lno(mut self, lno: usize) -> Self {
        if self.lno == 0 {
            self.lno = lno
        }
        self
    }
}

impl From<conl::SyntaxError> for Error {
    fn from(e: conl::SyntaxError) -> Self {
        Error {
            lno: e.lno,
            msg: e.msg,
        }
    }
}

impl serde::ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error {
            msg: format!("{}", msg),
            lno: 0,
        }
    }
}

impl serde::de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error {
            msg: format!("{}", msg),
            lno: 0,
        }
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if self.lno > 0 {
            formatter.write_fmt(format_args!("{}:", self.lno))?;
        }
        formatter.write_str(&self.msg)
    }
}

impl std::error::Error for Error {}
