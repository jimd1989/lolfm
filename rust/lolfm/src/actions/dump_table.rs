use std::io;

use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::models::table_name::TableName;
use crate::repos::loved_songs_from_db;
use crate::repos::played_songs_from_db;
use crate::repos::songs_from_db;
use crate::traits::cmus_encoder::CmusEncoder;

pub fn run(ω: &AppConfig, t: TableName, as_events: bool) -> Result<(), Er> {
  let stdout = io::stdout();
  let mut out = stdout.lock();
  match t {
    TableName::Loved => {
      let ss = loved_songs_from_db::get(&ω.db)?;
      for s in ss {
        s?.encode(&mut out, as_events)?;
      }
      Ok(())
    }
    TableName::Plays => {
      let ss = played_songs_from_db::get(&ω.db)?;
      for s in ss {
        s?.encode(&mut out, as_events)?;
      }
      Ok(())
    }
    TableName::Songs => {
      let ss = songs_from_db::get(&ω.db)?;
      for s in ss {
        s?.encode(&mut out, as_events)?;
      }
      Ok(())
    }
  }
}
