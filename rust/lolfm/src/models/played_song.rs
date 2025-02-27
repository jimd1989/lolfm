use std::io::Write;

use crate::models::er::Er;
use crate::models::timestamp::Seconds;
use crate::traits::cmus_encoder::CmusEncoder;

pub struct PlayedSong {
  pub date:         Seconds,
  pub artist:       String,
  pub album_artist: String,
  pub title:        String,
  pub album:        String,
  pub genre:        String,
  pub year:         i64,
  pub duration:     i64,
}

impl CmusEncoder for PlayedSong {
  fn as_event(&self, ω: &mut dyn Write) -> Result<(), Er> {
    let n = self.date.to_milliseconds().to_i64() - 1;
    Ok(writeln!(ω, 
"status playing
duration {}
tag artist {}
tag album {}
tag title {}
tag date {}
tag genre {}
tag albumartist {}
timemilliseconds {}
status stopped
timemilliseconds {}",
      self.duration, self.artist, self.album, self.title, self.year,
      self.genre, self.album_artist, n, self.date.to_milliseconds().to_i64())?)
  }
  fn as_row(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}", 
        self.date.to_i64(), self.artist, self.title, self.album, 
        self.year, self.album_artist, self.genre, self.duration)?)
  }
}
