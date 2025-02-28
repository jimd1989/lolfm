use std::io::Write;

use crate::models::er::Er;
use crate::models::timestamp::Seconds;
use crate::traits::cmus_event_encoder::CmusEventEncoder;
use crate::traits::row_encoder::RowEncoder;

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

/* Need date */
impl CmusEventEncoder for LovedSong {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "song\ntime {}\ntag artist {}\ntag title {}",
        self.date.to_i64(), self.artist, self.title)?)
  }
}

impl RowEncoder for LovedSong {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "{}\t{}\t{}", self.song_id, self.artist, self.title)?)
  }
}
