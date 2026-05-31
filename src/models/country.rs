use std::io::Write;

use crate::models::er::Er;
use crate::traits::row_encoder::RowEncoder;

#[derive(Debug)]
pub struct Country {
  pub abbreviation: String,
  pub name:         String,
}

impl Default for Country {
  fn default() -> Self {
    Self {
      abbreviation: "XX".to_string(),
      name: "Unknown Country".to_string(),
    }
  }
}

impl RowEncoder for Country {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "{}\t{}", self.abbreviation, self.name)?)
  }
}
