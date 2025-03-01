use std::iter;
use std::mem;

use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;

/* What a horrible mess. This returns vector groupings of input lines, reified
 * into CmusTag objects. It must simultaneously break up groups on a specific
 * `delimiter`, while keeping that `delimiter` within the next vector grouping
 * for potential further parsing. The strange statefulness and memory trickery
 * stem from these requirements. */

pub fn run<'a>
(mut ω: impl Iterator<Item = Result<String, Er>> + 'a, delimiter: &'a str) 
-> impl Iterator<Item = Vec<CmusTag>> + 'a {
  let mut group = Vec::new();
  iter::from_fn(move || {
    for line in ω.by_ref() {
      let l = line.ok()?;
      if l.starts_with(delimiter) && !group.is_empty() { 
        let returned_group = mem::take(&mut group);
        group.push(CmusTag::from_string(l));
        return Some(returned_group); 
      }
      if !l.starts_with("set") {
        group.push(CmusTag::from_string(l));
      }
    }
    if group.is_empty() { None } 
    else                { Some(mem::take(&mut group)) }
  })
}
