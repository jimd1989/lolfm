use crate::models::cmus_decoder::CmusDecoder;
use crate::models::cmus_event::CmusEvent;
use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::models::timestamp::Timestamp;

pub fn run(tags: impl Iterator<Item = Vec<CmusTag>>, time: Timestamp) 
-> impl Iterator<Item = Result<CmusEvent, Er>> {
  tags.map(move |ω| read_tags(ω, time))
}

fn read_tags(tags: Vec<CmusTag>, time: Timestamp) -> Result<CmusEvent, Er> {
  let mut e = CmusEvent::default().with_time(time);
  e.decode(tags)?;
  Ok(e)
}
