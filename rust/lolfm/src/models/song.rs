#[derive(Debug, PartialEq)]
pub struct Song {
  pub artist:             String,
  pub album_artist:       String,
  pub title:              String,
  pub album:              String,
  pub genre:              String,
  pub duration:           i64,
  pub year:               i64,
}

impl Default for Song {
  fn default() -> Self {
    Self {
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
