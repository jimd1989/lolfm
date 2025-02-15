use crate::models::cmus_status::CmusStatus;
use crate::models::played_song::PlayedSong;
use crate::models::played_songs::PlayedSongs;
use crate::models::raw_cmus_event::RawCmusEvent;

pub fn source_events(es: Vec<RawCmusEvent>, time_milliseconds: i64) { }

fn compare_events(t: i64, ps: &mut PlayedSongs, e: RawCmusEvent) {
  let is_future = e.time_milliseconds.map(|tt| t < tt).unwrap_or(false);
  if is_future { return; }
  let prev_status = ps.last_event.as_ref().map(|ω| &ω.status);
  match (prev_status, &e.status) {
    (_, _) => {}
  }
}

/* Need RawCmusEvent → Played Song */
/* Need to reconcile artist vs album_artist? */
