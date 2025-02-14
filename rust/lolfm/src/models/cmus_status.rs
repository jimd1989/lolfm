use sqlite::Error;

#[derive(Debug)]
pub enum CmusStatus {
  Stopped,
  Paused,
  Playing
}

impl CmusStatus {
  pub fn from_str(α: &str) -> Result<Self, String> {
    match α {
      "stopped" => Ok(CmusStatus::Stopped),
      "paused"  => Ok(CmusStatus::Paused),
      "playing" => Ok(CmusStatus::Playing),
      ω         => Err(format!("invalid status {}", ω).to_string())
    }
  }

  pub fn from_sql_enum(n: i64) -> Result<Self, Error> {
    match n {
      0 => Ok(CmusStatus::Stopped),
      1 => Ok(CmusStatus::Paused),
      2 => Ok(CmusStatus::Playing),
      ω => Err(Error {code: None, message: Some(format!("invalid enum {}", ω))})
    }
  }

  pub fn to_sql_enum(self) -> i64 {
    match self {
      CmusStatus::Stopped => 0,
      CmusStatus::Paused  => 1,
      CmusStatus::Playing => 2
    }
  }
}
