use crate::models::cmus_status::CmusStatus;
use crate::models::played_song::PlayedSong;
use crate::models::played_songs::PlayedSongs;
use crate::models::raw_cmus_event::RawCmusEvent;

pub fn run(es: Vec<RawCmusEvent>, time_milliseconds: i64) -> PlayedSongs {
  let mut ps = PlayedSongs::default();
  for e in es {
    /* Don't process future events */
    if e.time_milliseconds > time_milliseconds { return ps; }
    compare_events(e, &mut ps);
  }
  ps
}

fn compare_events(e: RawCmusEvent, ps: &mut PlayedSongs) {
  match (&ps.last_event, &e.status) {
    (None, CmusStatus::Paused | CmusStatus::Playing) => {
      /* Begin tracking new play */
      ps.cutoff_time = Some(e.time_milliseconds);
      ps.last_event = Some(e);
    }
    (Some(ω), CmusStatus::Paused | CmusStatus::Playing) => {
      if ω == &e {
        /* Continue tracking same play through pause/start changes */
        ps.cutoff_time = Some(e.time_milliseconds);
      } else { 
        /* Push finished play; begin tracking next play */
        ps.plays.push(to_played_song(&ω, e.time_milliseconds));
        ps.cutoff_time = Some(e.time_milliseconds);
        ps.last_event = Some(e);
      }
    }
    (Some(ω), CmusStatus::Stopped) => {
      /* Push finished play */
      ps.plays.push(to_played_song(&ω, e.time_milliseconds));
      ps.cutoff_time = Some(e.time_milliseconds);
      ps.last_event = None;
    }
    _ => {
      /* Erase through absurd conditions */
      ps.cutoff_time = Some(e.time_milliseconds);
    }
  }
}

fn to_played_song(e: &RawCmusEvent, t: i64) -> PlayedSong {
  let mut p = PlayedSong::default();
  assign_artists(e, &mut p);
  assign_year(&e.date, &mut p);
  p.time_milliseconds = t;
  e.title.as_ref().map(|ω| { p.title = ω.to_string() });
  e.album.as_ref().map(|ω| { p.album = ω.to_string() });
  e.genre.as_ref().map(|ω| { p.genre = ω.to_string() });
  p.duration = e.duration;
  p
}

fn assign_artists(e: &RawCmusEvent, p: &mut PlayedSong) {
  match (&e.artist, &e.album_artist) {
    (Some(α), Some(ω)) => {
      p.artist = α.to_string();
      p.album_artist = ω.to_string();
    }
    (Some(α), None) => {
      p.artist = α.to_string();
      p.album_artist = α.to_string();
    }
    (None, Some(ω)) => {
      p.album_artist = ω.to_string();
    }
    _ => {}
  }
}

fn assign_year(d: &Option<String>, p: &mut PlayedSong) {
  let y = d.as_ref().and_then(|ω| ω.parse::<i64>().ok()).unwrap_or(0);
  p.year = y;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_future_events() {
      let input = Vec::from([
        RawCmusEvent::default().with_time(1),
        RawCmusEvent::default().with_time(2),
      ]);
      let expected = PlayedSongs {
        cutoff_time: Some(1),
        last_event: None,
        plays: Vec::<PlayedSong>::new(),
      };
      let res = source_events(input, 1);
      assert_eq!(res, expected);
    }

    #[test]
    fn record_at_stop() {
      let input = Vec::from([
        RawCmusEvent::default().with_time(0).with_status(CmusStatus::Stopped),
        RawCmusEvent::default().with_time(1).with_status(CmusStatus::Playing),
        RawCmusEvent::default().with_time(2).with_status(CmusStatus::Paused),
        RawCmusEvent::default().with_time(3).with_status(CmusStatus::Playing),
        RawCmusEvent::default().with_time(4).with_status(CmusStatus::Paused),
        RawCmusEvent::default().with_time(5).with_status(CmusStatus::Stopped),
        RawCmusEvent::default().with_time(6).with_status(CmusStatus::Stopped),
        RawCmusEvent::default().with_time(7).with_status(CmusStatus::Playing),
        RawCmusEvent::default().with_time(8).with_status(CmusStatus::Stopped),
        RawCmusEvent::default().with_time(9).with_status(CmusStatus::Stopped),
      ]);
      let expected = PlayedSongs {
        cutoff_time: Some(9),
        last_event: None,
        plays: Vec::from([
          PlayedSong::default().with_time(5),
          PlayedSong::default().with_time(8)
        ]),
      };
      let res = source_events(input, 10);
      assert_eq!(res, expected);
    }

    #[test]
    fn record_at_song_change() {
      let e1 = RawCmusEvent::default().with_time(1)
                                      .with_status(CmusStatus::Playing)
                                      .with_track("A1".to_string());
      let e2 = RawCmusEvent::default().with_time(2)
                                      .with_status(CmusStatus::Playing)
                                      .with_track("A2".to_string());
      let input = Vec::from([e1, e2.clone()]);
      let expected = PlayedSongs {
        cutoff_time: Some(2),
        last_event: Some(e2),
        plays: Vec::from([PlayedSong::default().with_time(2)]),
      };
      let res = source_events(input, 3);
      assert_eq!(res, expected);
    }
}
