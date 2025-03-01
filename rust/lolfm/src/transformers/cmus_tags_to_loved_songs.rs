use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::models::loved_song::LovedSong;
use crate::traits::cmus_event_decoder::CmusEventDecoder;

pub fn run(tags: impl Iterator<Item = Vec<CmusTag>>) 
  -> impl Iterator<Item = Result<LovedSong, Er>> {
  tags.map(move |ω| read_tags(ω))
}

fn read_tags(tags: Vec<CmusTag>) -> Result<LovedSong, Er> {
  let mut s = LovedSong::default();
  s.decode(tags)?;
  Ok(s)
}
