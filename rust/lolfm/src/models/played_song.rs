#[derive(Debug, PartialEq)]
pub struct PlayedSong {
  pub time_milliseconds:  i64,
  pub artist:             String,
  pub album_artist:       String,
  pub title:              String,
  pub album:              String,
  pub genre:              String,
  pub duration:           i64,
  pub year:               i64,
}

impl Default for PlayedSong {
  fn default() -> Self {
    Self {
      time_milliseconds: 0,
      artist: "Unknown Artist".to_string(),
      album_artist: "Unknown Artist".to_string(),
      title: "Unknown Title".to_string(),
      album: "Unknown Album".to_string(),
      genre: "None".to_string(),
      duration: 0,
      year: 0,
    }
  }
}

impl PlayedSong {
  #[cfg(test)] 
  pub fn with_time(mut self, ω: i64) -> Self {
    self.time_milliseconds = ω;
    self
  }
}
