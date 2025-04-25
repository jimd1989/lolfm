use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::cmus_events_from_db;
use crate::repos::cmus_events_to_db;
use crate::repos::lines_from_cmus_remote;
use crate::repos::lolfm_events_to_db;
use crate::transformers::cmus_events_to_lolfm_events;
use crate::transformers::cmus_tags_to_cmus_events;
use crate::transformers::lines_to_cmus_tags;

pub fn run(ω: &AppConfig) -> Result<(), Er> {
  match process(ω) {
    Err(α) => { ω.db.execute("ROLLBACK")?; Err(α) }
    Ok(_)  => { Ok(ω.db.execute("COMMIT")?)       }
  }
}

fn process(ω: &AppConfig) -> Result<(), Er> {
           ω.db.execute("BEGIN TRANSACTION")?;
  let ls = lines_from_cmus_remote::get(&["-Q"])?;
  let ts = lines_to_cmus_tags::run(ls, "status");
  let cs = cmus_tags_to_cmus_events::run(ts, ω.time);
           cmus_events_to_db::write(&ω.db, cs)?;
  let es = cmus_events_from_db::get(&ω.db, ω.time)?;
  let ss = cmus_events_to_lolfm_events::run(es);
           lolfm_events_to_db::write(&ω.db, ss)
}
