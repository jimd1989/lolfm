use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::db_connection::connect_to_db;
use crate::repos::system_time::get_time_milliseconds;

pub fn get_app_config(db_path: &String) -> Result<AppConfig, Er> {
  let time_milliseconds  = get_time_milliseconds()?;
  let db = connect_to_db(&db_path)?;
  Ok(AppConfig { time_milliseconds, db })
}
