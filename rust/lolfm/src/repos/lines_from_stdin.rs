use std::io::{BufRead, stdin};

use crate::models::er::Er;

pub fn get() -> Result<impl Iterator<Item = Result<String, Er>>, Er> { 
  Ok(stdin().lock().lines().map(|α| α.map_err(Er::from)))
}
