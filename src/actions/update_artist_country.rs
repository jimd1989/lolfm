use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::update_artist_country_in_db;

pub fn run(ω: &AppConfig, artist_id: i64, abbreviation: String) 
-> Result<(), Er> {
  update_artist_country_in_db::write(&ω.db, artist_id, abbreviation)
}
