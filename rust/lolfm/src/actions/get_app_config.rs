use crate::models::app_config::AppConfig;
use crate::models::er::Er;
use crate::repos::db_connection;
use crate::repos::system_time;

pub fn get_app_config(db_path: &String) -> Result<AppConfig, Er> {
  let time = system_time::get()?;
  let db = db_connection::get(&db_path)?;
  Ok(AppConfig { time, db })
}
