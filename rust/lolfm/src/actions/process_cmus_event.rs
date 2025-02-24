use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::cmus_events_from_db;
use crate::repos::cmus_events_to_db;
use crate::repos::cmus_tags_from_shell;
use crate::repos::lolfm_events_to_db;
use crate::transformers::cmus_events_to_lolfm_events;
use crate::transformers::cmus_tags_to_cmus_events;

pub fn run(ω: &AppConfig) -> Result<(), Er> {
  match process(ω) {
    Err(α) => { ω.db.execute("ROLLBACK")?; Err(α) }
    Ok(_)  => { Ok(ω.db.execute("COMMIT")?)       }
  }
}

pub fn process(ω: &AppConfig) -> Result<(), Er> {
  ω.db.execute("BEGIN TRANSACTION")?;
  let ts = cmus_tags_from_shell::get("-Q", "status")?;
  let cs = cmus_tags_to_cmus_events::run(ts, ω.time);
           cmus_events_to_db::write(&ω.db, cs)?;
  let es = cmus_events_from_db::get(&ω.db)?;
  let ss = cmus_events_to_lolfm_events::run(es, ω.time);
           lolfm_events_to_db::write(&ω.db, ss)
}
