use std::io::Write;

use crate::models::er::Er;

pub trait CmusEncoder {
  fn as_event(&self, ω: &mut dyn Write) -> Result<(), Er>;
  fn as_row(&self, ω: &mut dyn Write) -> Result<(), Er>;
  fn encode(&self, ω: &mut dyn Write, as_event: bool) -> Result<(), Er> {
    if as_event { self.as_event(ω) } 
    else        { self.as_row(ω)   }?;
    Ok(ω.flush()?)
  }
}
