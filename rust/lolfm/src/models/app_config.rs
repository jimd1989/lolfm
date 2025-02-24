use sqlite::Connection;

use crate::models::timestamp::Timestamp;

pub struct AppConfig {
  pub time: Timestamp,
  pub db:   Connection,
}
