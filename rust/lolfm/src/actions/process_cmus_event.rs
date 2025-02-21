use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use
crate::repos::raw_cmus_events_delete_from_db::delete_raw_cmus_events_from_db;
use crate::repos::raw_cmus_events_from_db::get_raw_cmus_events_from_db;
use crate::repos::raw_cmus_event_from_shell::get_raw_cmus_event_from_shell;
use crate::repos::raw_cmus_event_to_db::write_raw_cmus_event_to_db;
use crate::transformers::raw_cmus_events_to_played_songs::source_events;

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
  let e = get_raw_cmus_event_from_shell(ω.time_milliseconds)?;
  write_raw_cmus_event_to_db(&ω.db, e)?;
  let es = get_raw_cmus_events_from_db(&ω.db)?;
  let ps = source_events(es, ω.time_milliseconds);
  /* Write plays here */
  match ps.cutoff_time {
    Some(t) => delete_raw_cmus_events_from_db(&ω.db, t),
    _       => Ok(()),
  }
}
