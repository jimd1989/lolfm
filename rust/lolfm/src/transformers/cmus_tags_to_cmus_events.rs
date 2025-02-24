use crate::models::cmus_event::CmusEvent;
use crate::models::cmus_status::CmusStatus;
use crate::models::er::Er;
use crate::models::timestamp::Timestamp;

pub fn run(tags: impl Iterator<Item = Vec<String>>, time: Timestamp) 
-> impl Iterator<Item = Result<CmusEvent, Er>> {
  tags.map(move |ω| read_tags(&ω, time))
}

fn read_tags(lines: &Vec<String>, time: Timestamp)
-> Result<CmusEvent, Er> {
  let mut e = CmusEvent::default().with_time(time);
  for l in lines { read_tag(&mut e, &l)?; }
  Ok(e)
}

fn read_tag(e: &mut CmusEvent, l: &String) -> Result<(), Er> {
  let mut s1  = l.splitn(2, ' ');
  let s1_head = s1.next();
  let s1_tail = s1.next();
  let mut s2  = s1_tail.map(|s| s.splitn(2, ' '));
  let s2_head = s2.as_mut().and_then(|ω| ω.next());
  let s2_tail = s2.as_mut().and_then(|ω| ω.next());
  match (s1_head, s2_head, s2_tail) {
    (Some("timemilliseconds"), Some(n), _) => { 
      let ω = parse_int(n)?;
      Ok({ e.time = Timestamp::from_milliseconds(ω); })
    },
    (Some("status"), Some(α), _) => {
      let ω = CmusStatus::from_str(α)?;
      Ok({ e.status = ω; })
    },
    (Some("tag"), Some("artist"), Some(ω)) => { 
      Ok({ e.artist = Some(ω.to_string()); })
    },
    (Some("tag"), Some("title"), Some(ω)) => { 
      Ok({ e.title = Some(ω.to_string()); })
    },
    (Some("tag"), Some("album"), Some(ω)) => { 
      Ok({ e.album = Some(ω.to_string()); })
    },
    (Some("tag"), Some("albumartist"), Some(ω)) => { 
      Ok({ e.album_artist = Some(ω.to_string()); })
    },
    (Some("tag"), Some("genre"), Some(ω)) => { 
      Ok({ e.genre = Some(ω.to_string()); })
    },
    (Some("tag"), Some("discnumber"), Some(ω)) => { 
      Ok({ e.disc_number = Some(ω.to_string()); })
    },
    (Some("tag"), Some("tracknumber"), Some(ω)) => { 
      Ok({ e.track_number = Some(ω.to_string()); })
    },
    (Some("duration"), Some(n), _) => { 
      let ω = parse_int(n)?;
      Ok({ e.duration = ω; })
    },
    (Some("tag"), Some("date"), Some(ω)) => { 
      Ok({ e.date = Some(ω.to_string()); })
    },
    _ => Ok(()),
  }
}

fn parse_int(s: &str) -> Result<i64, Er> {
  Ok(s.parse::<i64>()?)
}
