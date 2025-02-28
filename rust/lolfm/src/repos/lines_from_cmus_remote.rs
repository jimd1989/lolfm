use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};

use crate::models::er::Er;

pub fn get(args: &[&str]) 
  -> Result<impl Iterator<Item = Result<String, Er>>, Er> { 
  let α = Command::new("cmus-remote").args(args)
                                     .stdout(Stdio::piped()).spawn()?;
  let ω = α.stdout.ok_or("is cmus running?")?;
  Ok(BufReader::new(ω).lines().map(|α| α.map_err(Er::from)))
}
