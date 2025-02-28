use std::iter;

use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;

pub fn run<'a>
(mut ω: impl Iterator<Item = Result<String, Er>> + 'a, delimiter: &'a str) 
-> impl Iterator<Item = Vec<CmusTag>> + 'a {
  iter::from_fn(move || {
    let mut group = Vec::new();
    for line in ω.by_ref() {
      let l = line.ok()?;
      if l.starts_with(delimiter) && !group.is_empty() { return Some(group); }
      if !l.starts_with("set") {
        group.push(CmusTag::from_string(l));
      }
    }
    if group.is_empty() { None } 
    else                { Some(group) }
  })
}
