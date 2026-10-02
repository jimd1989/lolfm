use std::io::Write;

use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::traits::row_encoder::RowEncoder;
use crate::traits::cmus_event_decoder::CmusEventDecoder;
use crate::traits::cmus_event_encoder::CmusEventEncoder;

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

impl CmusEventDecoder for Artist {
  fn match_tag(&mut self, ω: CmusTag) -> Result<(), Er> {
    match (ω.0.as_ref().map(|α| α.as_str()), 
           ω.1.as_ref().map(|α| α.as_str()),
           ω.2.as_ref().map(|α| α.as_str())) {
      (Some("tag"), Some("artist"), Some(α)) => { 
        Ok({ self.name = α.to_string(); })
      },
      (Some("tag"), Some("country"), Some(α)) => { 
        Ok({ self.country_abbreviation = α.to_string(); })
      },
      _ => Ok(()),
    }
  }
}

impl CmusEventEncoder for Artist {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "country\ntag artist {}\ntag country {}",
        self.name, self.country_abbreviation)?)
  }
}

impl RowEncoder for Artist {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "{}\t{}\t{}\t{}",
       self.id, self.name, self.country_abbreviation, self.country)?)
  }
}
