use crate::models::cmus_decoder::CmusDecoder;
use crate::models::cmus_status::CmusStatus;
use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::models::timestamp::Timestamp;

#[derive(Clone, Debug)]
pub struct CmusEvent {
  pub time:         Timestamp,
  pub status:       CmusStatus,
  pub artist:       Option<String>,
  pub title:        Option<String>,
  pub album:        Option<String>,
  pub album_artist: Option<String>,
  pub genre:        Option<String>,
  pub disc_number:  Option<String>,
  pub track_number: Option<String>,
  pub duration:     i64,
  pub date:         Option<String>,
}

impl Default for CmusEvent {
  fn default() -> Self {
    Self {
      time: Timestamp::from_milliseconds(0),
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

impl CmusEvent {
  pub fn with_time(mut self, ω: Timestamp) -> Self {
    self.time = ω;
    self
  }
}

impl CmusDecoder for CmusEvent {
  fn match_tag(&mut self, ω: CmusTag) -> Result<(), Er> {
    match (ω.0.as_ref().map(|α| α.as_str()), 
           ω.1.as_ref().map(|α| α.as_str()),
           ω.2.as_ref().map(|α| α.as_str())) {
      (Some("timemilliseconds"), Some(n), _) => { 
        let ω = n.parse::<i64>()?;
        Ok({ self.time = Timestamp::from_milliseconds(ω); })
      },
      (Some("status"), Some(α), _) => {
        let ω = CmusStatus::from_str(α)?;
        Ok({ self.status = ω; })
      },
      (Some("tag"), Some("artist"), Some(ω)) => { 
        Ok({ self.artist = Some(ω.to_string()); })
      },
      (Some("tag"), Some("title"), Some(ω)) => { 
        Ok({ self.title = Some(ω.to_string()); })
      },
      (Some("tag"), Some("album"), Some(ω)) => { 
        Ok({ self.album = Some(ω.to_string()); })
      },
      (Some("tag"), Some("albumartist"), Some(ω)) => { 
        Ok({ self.album_artist = Some(ω.to_string()); })
      },
      (Some("tag"), Some("genre"), Some(ω)) => { 
        Ok({ self.genre = Some(ω.to_string()); })
      },
      (Some("tag"), Some("discnumber"), Some(ω)) => { 
        Ok({ self.disc_number = Some(ω.to_string()); })
      },
      (Some("tag"), Some("tracknumber"), Some(ω)) => { 
        Ok({ self.track_number = Some(ω.to_string()); })
      },
      (Some("duration"), Some(n), _) => { 
        let ω = n.parse::<i64>()?;
        Ok({ self.duration = ω; })
      },
      (Some("tag"), Some("date"), Some(ω)) => { 
        Ok({ self.date = Some(ω.to_string()); })
      },
      _ => Ok(()),
    }
  }
}

impl PartialEq for CmusEvent {
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
