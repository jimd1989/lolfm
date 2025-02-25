use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;

pub trait CmusDecoder {
  fn match_tag(&mut self, ω: CmusTag) -> Result<(), Er>;
  fn decode(&mut self, ω: Vec<CmusTag>) -> Result<(), Er> {
    for α in ω {
      self.match_tag(α)?;
    }
    Ok(())
  }
}
