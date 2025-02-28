use std::io::Write;

use crate::models::cmus_tag::CmusTag;
use crate::models::er::Er;
use crate::traits::cmus_event_decoder::CmusEventDecoder;
use crate::traits::row_encoder::RowEncoder;

#[derive(Debug, PartialEq)]
pub struct Song {
  pub id:           i64,    /* Only used when dumping from DB */
  pub artist:       String,
  pub album_artist: String,
  pub title:        String,
  pub album:        String,
  pub genre:        String,
  pub duration:     i64,
  pub year:         i64,
}

impl Default for Song {
  fn default() -> Self {
    Self {
      id: 0,
      artist: "Unknown Artist".to_string(),
      album_artist: "Unknown Artist".to_string(),
      title: "Unknown Title".to_string(),
      album: "Unknown Album".to_string(),
      genre: "None".to_string(),
      duration: 0,
      year: 0,
    }
  }
}

impl CmusEventDecoder for Song {
  fn match_tag(&mut self, ω: CmusTag) -> Result<(), Er> {
    match (ω.0.as_ref().map(|α| α.as_str()), 
           ω.1.as_ref().map(|α| α.as_str()),
           ω.2.as_ref().map(|α| α.as_str())) {
      (Some("tag"), Some("artist"), Some(α)) => { 
        Ok({ self.artist = α.to_string(); })
      },
      (Some("tag"), Some("title"), Some(α)) => { 
        Ok({ self.title = α.to_string(); })
      },
      (Some("tag"), Some("album"), Some(α)) => { 
        Ok({ self.album = α.to_string(); })
      },
      (Some("tag"), Some("albumartist"), Some(α)) => { 
        Ok({ self.album_artist = α.to_string(); })
      },
      (Some("tag"), Some("genre"), Some(α)) => { 
        Ok({ self.genre = α.to_string(); })
      },
      (Some("duration"), Some(n), _) => { 
        let α = n.parse::<i64>()?;
        Ok({ self.duration = α; })
      },
      (Some("tag"), Some("date"), Some(n)) => { 
        let α = n.parse::<i64>().unwrap_or(0);
        Ok({ self.year = α; })
      },
      _ => Ok(()),
    }
  }
}

impl RowEncoder for Song {
  fn print(&self, ω: &mut dyn Write) -> Result<(), Er> {
    Ok(writeln!(ω, "{}\t{}\t{}", self.id, self.artist, self.title)?)
  }
}
