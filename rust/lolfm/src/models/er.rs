use std::fmt::{Display, Formatter, Result};
use std::num::ParseIntError;
use std::time::SystemTimeError;

#[derive(Debug, PartialEq)]
pub struct Er(pub String);

impl Display for Er {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "{}", self.0)
  }
}

impl std::error::Error for Er {}

impl From<&str> for Er {
  fn from(α: &str) -> Self {
    Er(α.to_string())
  }
}

impl From<String> for Er {
  fn from(α: String) -> Self {
    Er(α)
  }
}

impl From<std::io::Error> for Er {
  fn from(α: std::io::Error) -> Self {
    Er(α.to_string())
  }
}

impl From<sqlite::Error> for Er {
  fn from(α: sqlite::Error) -> Self {
    Er(α.message.unwrap_or("sql error".to_string()))
  }
}

impl From<SystemTimeError> for Er {
  fn from(α: SystemTimeError) -> Self {
    Er(α.to_string())
  }
}

impl From<ParseIntError> for Er {
  fn from(α: ParseIntError) -> Self {
    Er(α.to_string())
  }
}
