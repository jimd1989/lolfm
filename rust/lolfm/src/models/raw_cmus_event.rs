use crate::models::cmus_status::CmusStatus;

#[derive(Debug)]
pub struct RawCmusEvent {
  pub time_milliseconds:  Option<i64>,
  pub status:             CmusStatus,
  pub artist:             Option<String>,
  pub title:              Option<String>,
  pub album:              Option<String>,
  pub album_artist:       Option<String>,
  pub genre:              Option<String>,
  pub disc_number:        Option<String>,
  pub track_number:       Option<String>,
  pub duration:           i64,
  pub date:               Option<String>,
}

impl Default for RawCmusEvent {
  fn default() -> Self {
    Self {
      time_milliseconds: None,
      status: CmusStatus::Stopped,
      artist: None,
      title: None,
      album: None,
      album_artist: None,
      genre: None,
      disc_number: None,
      track_number: None,
      duration: 0,
      date: None,
    }
  }
}

impl PartialEq for RawCmusEvent {
/* Ignores timestamp and status — useful in event sourcing */
  fn eq(&self, ω: &Self) -> bool {
    self.artist       == ω.artist       &&
    self.title        == ω.title        &&
    self.album        == ω.album        &&
    self.album_artist == ω.album_artist &&
    self.genre        == ω.genre        &&
    self.disc_number  == ω.disc_number  &&
    self.track_number == ω.track_number &&
    self.duration     == ω.duration     &&
    self.date         == ω.date
  }
}
