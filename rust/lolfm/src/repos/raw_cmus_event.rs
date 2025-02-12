use std::borrow::Cow;
use std::io::{BufRead, BufReader, Error, ErrorKind, Lines};
use std::process::{ChildStdout, Command, Stdio};

#[derive(Debug)]
pub struct RawCmusEvent {
  status: Option<Cow<'static, str>>,
  artist: Option<Cow<'static, str>>,
  title: Option<Cow<'static, str>>,
  album: Option<Cow<'static, str>>,
  album_artist: Option<Cow<'static, str>>,
  genre: Option<Cow<'static, str>>,
  duration: usize,
  date: usize
}

pub fn get_raw_cmus_event() -> Result<RawCmusEvent, String> {
  let mut event = RawCmusEvent {
    status: None,
    artist: None,
    title: None,
    album: None,
    album_artist: None,
    genre: None,
    duration: 0,
    date: 0
  };
  let mut lines = get_lines()?;
  let res = lines.try_for_each(|line| {
    let l = line.map_err(|e| e.to_string())?;
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
  let s2_head = s2.as_mut().and_then(|s| s.next());
  let s2_tail = s2.as_mut().and_then(|s| s.next());
  match (s1_head, s2_head, s2_tail) {
    (Some("status"), Some(a), _) => {
      e.status = Some(Cow::Owned(a.to_string())); Ok(())
    },
    (Some("tag"), Some("artist"), Some(a)) => { 
      e.artist = Some(Cow::Owned(a.to_string())); Ok(())
    },
    (Some("tag"), Some("album"), Some(a)) => { 
      e.album = Some(Cow::Owned(a.to_string())); Ok(())
    },
    (Some("tag"), Some("albumartist"), Some(a)) => { 
      e.album_artist = Some(Cow::Owned(a.to_string())); Ok(())
    },
    (Some("tag"), Some("title"), Some(a)) => { 
      e.title = Some(Cow::Owned(a.to_string())); Ok(())
    },
    (Some("tag"), Some("genre"), Some(a)) => { 
      e.genre = Some(Cow::Owned(a.to_string())); Ok(())
    },
    (Some("duration"), Some(n), _) => { 
      parse_int(n).map(|m| {e.duration = m; ()})
    },
    (Some("tag"), Some("date"), Some(n)) => { 
      parse_int(n).map(|m| {e.date = m; ()})
    },
    _ => Ok(()),
  }
}

fn parse_int(s: &str) -> Result<usize, String> {
  let n = s.parse::<usize>();
  n.map_err(|_| "expected number for duration/date".to_string())
}

fn get_lines() -> Result<Lines<BufReader<ChildStdout>>, String> { 
  let err = Error::new(ErrorKind::Other, "is cmus running?");
  Command::new("cmus-remote").arg("-Q").stdout(Stdio::piped()).spawn()
    .and_then(|c| c.stdout.ok_or(err))
    .map(|o| BufReader::new(o).lines())
    .map_err(|e| e.to_string())
}
