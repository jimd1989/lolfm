use std::io::{BufRead, BufReader, Error, ErrorKind, Lines};
use std::process::{ChildStdout, Command, Stdio};

use crate::models::cmus_status;
use crate::models::raw_cmus_event::{RawCmusEvent, empty_raw_cmus_event};

pub fn get_raw_cmus_event_from_shell() -> Result<RawCmusEvent, String> {
  let mut event = empty_raw_cmus_event();
  let mut lines = get_lines()?;
  let res = lines.try_for_each(|line| {
    let l = line.map_err(|ω| ω.to_string())?;
    read_tag(&mut event, &l)?;
    Ok(())
  });
  res.map(|_| event)
}

fn read_tag(e: &mut RawCmusEvent, l: &String) -> Result<(), String> {
  let mut s1  = l.splitn(2, ' ');
  let s1_head = s1.next();
  let s1_tail = s1.next();
  let mut s2  = s1_tail.map(|s| s.splitn(2, ' '));
  let s2_head = s2.as_mut().and_then(|ω| ω.next());
  let s2_tail = s2.as_mut().and_then(|ω| ω.next());
  match (s1_head, s2_head, s2_tail) {
    (Some("status"), Some(α), _) => {
      cmus_status::from_str(α).map(|ω| {e.status = ω; ()})
    },
    (Some("tag"), Some("artist"), Some(ω)) => { 
      e.artist = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("album"), Some(ω)) => { 
      e.album = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("albumartist"), Some(ω)) => { 
      e.album_artist = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("title"), Some(ω)) => { 
      e.title = Some(ω.to_string()); Ok(())
    },
    (Some("tag"), Some("genre"), Some(ω)) => { 
      e.genre = Some(ω.to_string()); Ok(())
    },
    (Some("duration"), Some(n), _) => { 
      parse_int(n).map(|ω| {e.duration = ω; ()})
    },
    (Some("tag"), Some("date"), Some(n)) => { 
      parse_int(n).map(|ω| {e.date = ω; ()})
    },
    _ => Ok(()),
  }
}

fn parse_int(s: &str) -> Result<i64, String> {
  let n = s.parse::<i64>();
  n.map_err(|_| "expected number for duration/date".to_string())
}

fn get_lines() -> Result<Lines<BufReader<ChildStdout>>, String> { 
  let err = Error::new(ErrorKind::Other, "is cmus running?");
  Command::new("cmus-remote").arg("-Q").stdout(Stdio::piped()).spawn()
    .and_then(|ω| ω.stdout.ok_or(err))
    .map(|ω| BufReader::new(ω).lines())
    .map_err(|ω| ω.to_string())
}
