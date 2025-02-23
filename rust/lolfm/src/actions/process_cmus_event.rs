use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::cmus_tags_from_shell;
use crate::repos::lolfm_events_to_db;
use crate::repos::played_songs_to_db;
use crate::repos::raw_cmus_events_delete_from_db;
use crate::repos::raw_cmus_events_from_db;
use crate::repos::raw_cmus_events_to_db;
use crate::transformers::cmus_tags_to_raw_cmus_events;
use crate::transformers::raw_cmus_events_to_lolfm_events;
use crate::transformers::raw_cmus_events_to_played_songs;

pub fn run(ω: &AppConfig) -> Result<(), Er> {
  match process(ω) {
    Err(α) => { ω.db.execute("ROLLBACK")?; Err(α) }
    Ok(_)  => { Ok(ω.db.execute("COMMIT")?)       }
  }
}

pub fn process(ω: &AppConfig) -> Result<(), Er> {
  ω.db.execute("BEGIN TRANSACTION")?;
  let ts    =  cmus_tags_from_shell::get("-Q", "status")?;
  let tes   =  cmus_tags_to_raw_cmus_events::run(ts, ω.time_milliseconds);
               raw_cmus_events_to_db::write(&ω.db, tes)?;
  let ees   =  raw_cmus_events_from_db::get2(&ω.db)?;
  let ss    =  raw_cmus_events_to_lolfm_events::run(ees, ω.time_milliseconds);
               lolfm_events_to_db::write(&ω.db, ss)
}
