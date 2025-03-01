use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::love_song_in_db;

pub fn run(ω: &AppConfig, id: i64) -> Result<(), Er> {
  love_song_in_db::write(&ω.db, ω.time, id)
}
