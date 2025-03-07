use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::models::song::Song;
use crate::traits::cmus_event_decoder::CmusEventDecoder;

pub fn run(tags: impl Iterator<Item = Vec<CmusTag>>) 
  -> impl Iterator<Item = Result<Song, Er>> {
  tags.map(move |ω| read_tags(ω))
}

fn read_tags(tags: Vec<CmusTag>) -> Result<Song, Er> {
  let mut s = Song::default();
  s.decode(tags)?;
  Ok(s)
}
