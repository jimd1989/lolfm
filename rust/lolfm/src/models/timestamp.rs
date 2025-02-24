#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Timestamp(i64);

impl Timestamp {
  pub fn from_milliseconds(ω: i64) -> Self {
    Self(ω)
  }
  pub fn to_milliseconds(&self) -> i64 {
    self.0
  }
  pub fn to_seconds(&self) -> i64 {
    self.0 / 1000
  }
}
