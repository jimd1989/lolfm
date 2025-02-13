use crate::models::cmus_status::CmusStatus;

#[derive(Debug)]
pub struct RawCmusEvent {
  pub time: Option<i64>,
  pub status: CmusStatus,
  pub artist: Option<String>,
  pub title: Option<String>,
  pub album: Option<String>,
  pub album_artist: Option<String>,
  pub genre: Option<String>,
  pub duration: i64,
  pub date: i64,
}

pub fn empty_raw_cmus_event() -> RawCmusEvent {
  RawCmusEvent {
    time: None,
    status: CmusStatus::Stopped,
    artist: None,
    title: None,
    album: None,
    album_artist: None,
    genre: None,
    duration: 0,
    date: 0
  }
}
