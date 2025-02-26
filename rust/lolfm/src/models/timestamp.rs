#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Milliseconds(i64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Seconds(i64);

impl Milliseconds {
  pub fn from_i64(ω: i64) -> Self {
    Self(ω)
  }
  pub fn to_i64(&self) -> i64 {
    self.0
  }
  pub fn to_seconds(&self) -> Seconds {
    Seconds(self.0 / 1000)
  }
}

impl Seconds {
  pub fn from_i64(ω: i64) -> Self {
    Self(ω)
  }
  pub fn to_i64(&self) -> i64 {
    self.0
  }
}
