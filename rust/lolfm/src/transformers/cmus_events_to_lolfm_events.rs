use std::iter;

use crate::models::cmus_status::CmusStatus;
use crate::models::er::Er;
use crate::models::lolfm_event::LolfmEvent;
use crate::models::cmus_event::CmusEvent;
use crate::models::song::Song;
use crate::models::timestamp::Milliseconds;

pub fn run(mut es: impl Iterator<Item = Result<CmusEvent, Er>>, lim: Milliseconds)
-> impl Iterator<Item = Result<LolfmEvent, Er>> {
  /* Cutoff timestamp to DELETE outdated events when stream is finished */
  let mut cutoff = lim;
  /* Previous event in stream */
  let mut prev: Option<CmusEvent> = None;
  let mut stream_end = false;
  iter::from_fn(move || {
  /* There is a n:1 relationship between CmusEvents and LolfmEvents. The 
   * iterator must loop over multiple raw events and return completed songs at 
   * irregular intervals, hence this ugly stateful loop. */
    if stream_end {
      return None;
    }
    for res in es.by_ref() {
      match res {
        Ok(e)  => { 
          /* It is possible to write future events. If one manually queued up 
           * a 50 minute album's worth of plays at current time T, then events
           * would be written immediately out to T+50min. These do not get
           * logged as plays until T+50min has arrived, however. The stream
           * simply ends without deleting them. */
          if e.time > lim { return None; }
          cutoff = e.time;
          let (song, new_prev) = match (&prev, &e.status) {
            /* Begin tracking new play */
            (None, CmusStatus::Paused | CmusStatus::Playing) =>
              (None, Some(e)),
            (Some(ω), CmusStatus::Paused | CmusStatus::Playing) => {
              if ω == &e {
                /* Continue tracking same play through pause/start changes */
                (None, Some(e))
              } else { 
                /* Return finished play; begin tracking next play */
                (Some(to_song(&ω)), Some(e))
              }
            }
            /* Return finished play */
            (Some(ω), CmusStatus::Stopped) => (Some(to_song(&ω)), None),
            /* Write past absurd conditions */
            _ => (None, None),
          };
          /* Match on song, new_prev */
          prev = new_prev;
          match song {
            Some(ω) => { 
              return Some(Ok(LolfmEvent::RecordPlay(cutoff.to_seconds(), ω))); 
            }
            _       => {}
          }
        }
        Err(ω) => { return Some(Err(ω)); }
      }
    }
    /* Always return DELETE command at end of event stream */
    stream_end = true;
    Some(Ok(LolfmEvent::DeleteBefore(cutoff)))
  })
}

fn to_song(e: &CmusEvent) -> Song {
  let mut s = Song::default();
  assign_artists(e, &mut s);
  assign_year(&e.date, &mut s);
  e.title.as_ref().map(|ω| { s.title = ω.to_string() });
  e.album.as_ref().map(|ω| { s.album = ω.to_string() });
  e.genre.as_ref().map(|ω| { s.genre = ω.to_string() });
  s.duration = e.duration;
  s
}

fn assign_artists(e: &CmusEvent, s: &mut Song) {
  match (&e.artist, &e.album_artist) {
    (Some(α), Some(ω)) => {
      s.artist = α.to_string();
      s.album_artist = ω.to_string();
    }
    (Some(α), None) => {
      s.artist = α.to_string();
      s.album_artist = α.to_string();
    }
    (None, Some(ω)) => {
      s.album_artist = ω.to_string();
    }
    _ => {}
  }
}

fn assign_year(d: &Option<String>, s: &mut Song) {
  let y = d.as_ref().and_then(|ω| ω.parse::<i64>().ok()).unwrap_or(0);
  s.year = y;
}
