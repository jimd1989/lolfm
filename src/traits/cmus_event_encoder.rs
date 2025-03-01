use std::io::Write;

use crate::models::er::Er;

pub trait CmusEventEncoder {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er>;
  fn print_cmus_event(&self, ω: &mut dyn Write) -> Result<(), Er> {
    self.print(ω)?;
    Ok(ω.flush()?)
  }
}

