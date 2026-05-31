use std::io::Write;

use crate::models::er::Er;
use crate::traits::row_encoder::RowEncoder;

#[derive(Debug)]
pub struct Artist {
  pub id:   i64,
  pub name: String,
  pub country_abbreviation: String,
  pub country: String,
}

impl Default for Artist {
  fn default() -> Self {
    Self {
      id: 0,
      name: "Unknown Artist".to_string(),
      country_abbreviation: "XX".to_string(),
      country: "Unknown Country".to_string(),
    }
  }
}

impl RowEncoder for Artist {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "{}\t{}\t{}\t{}",
       self.id, self.name, self.country_abbreviation, self.country)?)
  }
}
