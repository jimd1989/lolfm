use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::models::artist::Artist;
use crate::traits::cmus_event_decoder::CmusEventDecoder;

pub fn run(tags: impl Iterator<Item = Vec<CmusTag>>) 
  -> impl Iterator<Item = Result<Artist, Er>> {
  tags.map(move |ω| read_tags(ω))
}

fn read_tags(tags: Vec<CmusTag>) -> Result<Artist, Er> {
  let mut s = Artist::default();
  s.decode(tags)?;
  Ok(s)
}
