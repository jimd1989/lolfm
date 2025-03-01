use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::loved_song_delete_from_db;

pub fn run(ω: &AppConfig, id: i64) -> Result<(), Er> {
  loved_song_delete_from_db::run(&ω.db, id)
}
