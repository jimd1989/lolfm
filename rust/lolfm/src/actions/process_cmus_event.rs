use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::played_songs_to_db;
use crate::repos::raw_cmus_events_delete_from_db;
use crate::repos::raw_cmus_events_from_db;
use crate::repos::raw_cmus_event_from_shell;
use crate::repos::raw_cmus_event_to_db;
use crate::transformers::raw_cmus_events_to_played_songs;

pub fn process_cmus_event(ω: &AppConfig) -> Result<(), Er> {
  match process(ω) {
    Err(α) => {
      ω.db.execute("ROLLBACK")?;
      Err(α)
    }
    Ok(_) => {
      Ok(ω.db.execute("COMMIT")?)
    }
  }
}

pub fn process(ω: &AppConfig) -> Result<(), Er> {
  ω.db.execute("BEGIN TRANSACTION")?;
  let e     =  raw_cmus_event_from_shell::get(ω.time_milliseconds)?;
               raw_cmus_event_to_db::write(&ω.db, e)?;
  let es    =  raw_cmus_events_from_db::get(&ω.db)?;
  let ps    =  raw_cmus_events_to_played_songs::run(es, ω.time_milliseconds);
               played_songs_to_db::write(&ω.db, ps.plays)?;
  match ps.cutoff_time {
    Some(t) => raw_cmus_events_delete_from_db::run(&ω.db, t),
    _       => Ok(()),
  }
}
