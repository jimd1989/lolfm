use crate::models::cmus_event::CmusEvent;
use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::models::timestamp::Milliseconds;
use crate::traits::cmus_decoder::CmusDecoder;

pub fn run(tags: impl Iterator<Item = Vec<CmusTag>>, time: Milliseconds) 
-> impl Iterator<Item = Result<CmusEvent, Er>> {
  tags.map(move |ω| read_tags(ω, time))
}

fn read_tags(tags: Vec<CmusTag>, time: Milliseconds) -> Result<CmusEvent, Er> {
  let mut e = CmusEvent::default().with_time(time);
  e.decode(tags)?;
  Ok(e)
}
