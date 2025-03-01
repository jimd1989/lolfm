use std::io::Write;

use crate::models::er::Er;

pub trait RowEncoder {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er>;
  fn print_row(&self, ω: &mut dyn Write) -> Result<(), Er> {
    self.print(ω)?;
    Ok(ω.flush()?)
  }
}
