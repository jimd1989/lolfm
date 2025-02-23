use std::io::{BufRead, BufReader, Lines};
use std::iter;
use std::process::{ChildStdout, Command, Stdio};

use crate::models::er::Er;

pub fn get<'a>(args: &str, delimiter: &'a str) 
-> Result<impl Iterator<Item = Vec<String>> + 'a, Er> { 
  let α = Command::new("cmus-remote").arg(args)
                                     .stdout(Stdio::piped()).spawn()?;
  let ω = α.stdout.ok_or("is cmus running?")?;
  Ok(grouped(BufReader::new(ω).lines(), delimiter))
}

pub fn grouped<'a>(mut ω: Lines<BufReader<ChildStdout>>, delimiter: &'a str)
-> impl Iterator<Item = Vec<String>> + 'a {
  iter::from_fn(move || {
    let mut group = Vec::new();
    for line in ω.by_ref() {
      let l = line.ok()?;
      if l.starts_with(delimiter) && !group.is_empty() { return Some(group); }
      group.push(l);
    }
    if group.is_empty() { None } 
    else                { Some(group) }
  })
}
