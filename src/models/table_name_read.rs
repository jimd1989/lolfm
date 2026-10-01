use crate::models::er::Er;

#[derive(Debug)]
pub enum ReadTableName {
  Loved,
  Plays,
  Artists
}

impl ReadTableName {
  pub fn from_string(ω: &str) -> Result<Self, Er> {
    match ω {
      α if α.eq_ignore_ascii_case("LOVED") => Ok(ReadTableName::Loved),
      α if α.eq_ignore_ascii_case("PLAYS") => Ok(ReadTableName::Plays),
      α if α.eq_ignore_ascii_case("ARTISTS") => Ok(ReadTableName::Artists),
      α => Err(format!("invalid table {}", α).into()),
    }
  }
}
