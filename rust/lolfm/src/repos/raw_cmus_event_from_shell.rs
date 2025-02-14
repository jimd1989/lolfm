use std::io::{BufRead, BufReader, Error, ErrorKind, Lines};
use std::time::{SystemTime, UNIX_EPOCH};
use std::process::{ChildStdout, Command, Stdio};

use crate::models::cmus_status::CmusStatus;
use crate::models::raw_cmus_event::RawCmusEvent;

pub fn get_raw_cmus_event_from_shell() -> Result<RawCmusEvent, String> {
  let mut event = RawCmusEvent::default();
  let mut lines = get_lines()?;
  let mut lines_read = 0;
  let res = lines.try_for_each(|line| {
    let l = line.map_err(|ω| ω.to_string())?;
    read_tag(&mut event, &l)?;
    lines_read += 1;
    Ok(())
  });
  match (lines_read, res) {
    (0,      _) => Err("is cmus running?".to_string()),
    (_, Err(ω)) => Err(ω),
    (_,  Ok(_)) => Ok(()),
  }?;
  set_time(&mut event)?;
  Ok(event)
}

fn read_tag(e: &mut RawCmusEvent, l: &String) -> Result<(), String> {
  let mut s1  = l.splitn(2, ' ');
  let s1_head = s1.next();
  let s1_tail = s1.next();
  let mut s2  = s1_tail.map(|s| s.splitn(2, ' '));
  let s2_head = s2.as_mut().and_then(|ω| ω.next());
  let s2_tail = s2.as_mut().and_then(|ω| ω.next());
  match (s1_head, s2_head, s2_tail) {
    (Some("time_milliseconds"), Some(n), _) => { 
      parse_int(n).map(|ω| {e.time_milliseconds = Some(ω); ()})
    },
    (Some("status"), Some(α), _) => {
      CmusStatus::from_str(α).map(|ω| {e.status = ω; ()})
    },
    (Some("tag"), Some("artist"), Some(ω)) => { 
      e.artist = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("title"), Some(ω)) => { 
      e.title = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("album"), Some(ω)) => { 
      e.album = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("albumartist"), Some(ω)) => { 
      e.album_artist = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("genre"), Some(ω)) => { 
      e.genre = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("discnumber"), Some(ω)) => { 
      e.disc_number = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("tracknumber"), Some(ω)) => { 
      e.track_number = Some(ω.to_string()); Ok(())
    },
    (Some("duration"), Some(n), _) => { 
      parse_int(n).map(|ω| {e.duration = ω; ()})
    },
    (Some("tag"), Some("date"), Some(ω)) => { 
      e.date = Some(ω.to_string()); Ok(())
    },
    _ => Ok(()),
  }
}

fn parse_int(s: &str) -> Result<i64, String> {
  let n = s.parse::<i64>();
  n.map_err(|_| "expected number for duration".to_string())
}

fn get_lines() -> Result<Lines<BufReader<ChildStdout>>, String> { 
  let err = Error::new(ErrorKind::Other, "is cmus running?");
  Command::new("cmus-remote").arg("-Q").stdout(Stdio::piped()).spawn()
    .and_then(|ω| ω.stdout.ok_or(err))
    .map(|ω| BufReader::new(ω).lines())
    .map_err(|ω| ω.to_string())
}

fn set_time(event: &mut RawCmusEvent) -> Result<(), String> {
  match event.time_milliseconds {
    Some(_) => Ok(()),
    None    => {
      let t = SystemTime::now().duration_since(UNIX_EPOCH)
        .map_err(|ω| ω.to_string())?;
      event.time_milliseconds = Some(t.as_millis() as i64);
      Ok(())
    }
  }
}
