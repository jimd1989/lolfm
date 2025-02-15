use crate::models::played_song::PlayedSong;
use crate::models::raw_cmus_event::RawCmusEvent;

#[derive(Debug)]
pub struct PlayedSongs {
  pub cutoff_time:  Option<i64>,
  pub last_event:   Option<RawCmusEvent>,
  pub plays:        Vec<PlayedSong>,
}

impl Default for PlayedSongs {
  fn default() -> Self {
    Self {
      cutoff_time: None,
      last_event: None,
      plays: Vec::new(),
    }
  }
}
