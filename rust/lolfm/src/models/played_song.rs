#[derive(Debug)]
pub struct PlayedSong {
  pub time_milliseconds:  i64,
  pub artist:             String,
  pub title:              String,
  pub album:              String,
  pub genre:              String,
  pub duration:           i64,
  pub date:               i64,
}

impl Default for PlayedSong {
  fn default() -> Self {
    Self {
      time_milliseconds: 0,
      artist: "Unknown Artist".to_string(),
      title: "Unknown Title".to_string(),
      album: "Unknown Album".to_string(),
      genre: "None".to_string(),
      duration: 0,
      date: 0,
    }
  }
}
