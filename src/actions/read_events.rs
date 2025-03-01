use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::models::table_name_read::ReadTableName;
use crate::repos::lines_from_stdin;
use crate::repos::lolfm_events_to_db;
use crate::repos::loved_songs_to_db;
use crate::transformers::cmus_events_to_lolfm_events;
use crate::transformers::cmus_tags_to_cmus_events;
use crate::transformers::cmus_tags_to_loved_songs;
use crate::transformers::lines_to_cmus_tags;

pub fn run(ω: &AppConfig, t: ReadTableName) -> Result<(), Er> {
  match process(ω, t) {
    Err(α) => { ω.db.execute("ROLLBACK")?; Err(α) }
    Ok(_)  => { Ok(ω.db.execute("COMMIT")?)       }
  }
}

fn process(ω: &AppConfig, t: ReadTableName) -> Result<(), Er> {
  ω.db.execute("BEGIN TRANSACTION")?;
  match t {
    ReadTableName::Plays => {
      let ls = lines_from_stdin::get()?;
      let ts = lines_to_cmus_tags::run(ls, "status");
      let cs = cmus_tags_to_cmus_events::run(ts, ω.time);
      let ss = cmus_events_to_lolfm_events::run(cs, ω.time);
               lolfm_events_to_db::write(&ω.db, ss)
    }
    ReadTableName::Loved => {
      let ls = lines_from_stdin::get()?;
      let ts = lines_to_cmus_tags::run(ls, "love");
      let ss = cmus_tags_to_loved_songs::run(ts);
               loved_songs_to_db::write(&ω.db, ss)
    }
  }
}
