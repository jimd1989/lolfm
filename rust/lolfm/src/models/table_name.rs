use crate::models::er::Er;

#[derive(Debug)]
pub enum TableName {
  Loved,
  Plays,
  Songs,
}

impl TableName {
  pub fn from_string(ω: &str) -> Result<Self, Er> {
    match ω {
      α if α.eq_ignore_ascii_case("LOVED") => Ok(TableName::Loved),
      α if α.eq_ignore_ascii_case("PLAYS") => Ok(TableName::Plays),
      α if α.eq_ignore_ascii_case("SONGS") => Ok(TableName::Songs),
      α => Err(format!("invalid table {}", α).into()),
    }
  }
}
