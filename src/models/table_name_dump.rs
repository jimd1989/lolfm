use crate::models::er::Er;

#[derive(Debug)]
pub enum DumpTableName {
  Loved,
  Plays,
  Songs,
  Countries,
}

impl DumpTableName {
  pub fn from_string(ω: &str) -> Result<Self, Er> {
    match ω {
      α if α.eq_ignore_ascii_case("LOVED") => Ok(DumpTableName::Loved),
      α if α.eq_ignore_ascii_case("PLAYS") => Ok(DumpTableName::Plays),
      α if α.eq_ignore_ascii_case("SONGS") => Ok(DumpTableName::Songs),
      α if α.eq_ignore_ascii_case("COUNTRIES") => Ok(DumpTableName::Countries),
      α => Err(format!("invalid table {}", α).into()),
    }
  }
}
