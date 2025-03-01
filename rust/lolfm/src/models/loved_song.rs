use std::io::Write;

use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::models::timestamp::Seconds;
use crate::traits::cmus_event_decoder::CmusEventDecoder;
use crate::traits::cmus_event_encoder::CmusEventEncoder;
use crate::traits::row_encoder::RowEncoder;

#[derive(Debug)]
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

impl CmusEventDecoder for LovedSong {
  fn match_tag(&mut self, ω: CmusTag) -> Result<(), Er> {
    match (ω.0.as_ref().map(|α| α.as_str()), 
           ω.1.as_ref().map(|α| α.as_str()),
           ω.2.as_ref().map(|α| α.as_str())) {
      (Some("tag"), Some("artist"), Some(α)) => { 
        Ok({ self.artist = α.to_string(); })
      },
      (Some("tag"), Some("title"), Some(α)) => { 
        Ok({ self.title = α.to_string(); })
      },
      (Some("time"), Some(n), _) => { 
        let α = n.parse::<i64>()?;
        Ok({ self.date = Seconds::from_i64(α); })
      },
      _ => Ok(()),
    }
  }
}

impl CmusEventEncoder for LovedSong {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "love\ntime {}\ntag artist {}\ntag title {}",
        self.date.to_i64(), self.artist, self.title)?)
  }
}

impl RowEncoder for LovedSong {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "{}\t{}\t{}", self.song_id, self.artist, self.title)?)
  }
}
