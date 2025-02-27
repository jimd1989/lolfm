use std::io::Write;

use crate::traits::cmus_encoder::CmusEncoder;
use crate::models::er::Er;
use crate::models::timestamp::Seconds;

pub struct LovedSong {
  pub date:     Seconds,
  pub song_id:  i64,
  pub artist:   String,
  pub title:    String,
}

impl Default for LovedSong {
  fn default() -> Self {
    Self {
      date: Seconds::from_i64(0),
      song_id: 0,
      artist: "Unknown Artist".to_string(),
      title: "Unknown Title".to_string(),
    }
  }
}

impl CmusEncoder for LovedSong {
  fn as_event(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "song\ntag id {}\ntag artist {}\ntag title {}",
       self.song_id, self.artist, self.title)?)
  }
  fn as_row(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "{}\t{}\t{}", self.song_id, self.artist, self.title)?)
  }
}
