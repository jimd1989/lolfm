use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::db_create_schema;
use crate::repos::lines_from_cmus_remote;
use crate::repos::songs_to_db;
use crate::transformers::cmus_tags_to_songs;
use crate::transformers::lines_to_cmus_tags;

pub fn run(ω: &AppConfig) -> Result<(), Er> {
  match process(ω) {
    Err(α) => { ω.db.execute("ROLLBACK")?; Err(α) }
    Ok(_)  => { Ok(ω.db.execute("COMMIT")?)       }
  }
}

fn process(ω: &AppConfig) -> Result<(), Er> {
           ω.db.execute("BEGIN TRANSACTION")?;
           db_create_schema::run(&ω.db)?;
  let ls = lines_from_cmus_remote::get(&["-C", "save -l -e -"])?;
  let ts = lines_to_cmus_tags::run(ls, "file");
  let ss = cmus_tags_to_songs::run(ts);
           songs_to_db::write(&ω.db, ss)
}
