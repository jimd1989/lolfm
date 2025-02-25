use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::cmus_tags_from_shell;
use crate::repos::db_create_schema;
use crate::repos::songs_to_db;
use crate::transformers::cmus_tags_to_songs;

pub fn run(ω: &AppConfig) -> Result<(), Er> {
  match process(ω) {
    Err(α) => { ω.db.execute("ROLLBACK")?; Err(α) }
    Ok(_)  => { Ok(ω.db.execute("COMMIT")?)       }
  }
}

fn process(ω: &AppConfig) -> Result<(), Er> {
           ω.db.execute("BEGIN TRANSACTION")?;
           db_create_schema::run(&ω.db)?;
  let ts = cmus_tags_from_shell::get(&["-C", "save -l -e -"], "file")?;
  let ss = cmus_tags_to_songs::run(ts);
           songs_to_db::write(&ω.db, ss)
}
