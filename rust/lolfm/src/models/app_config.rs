use sqlite::Connection;

use crate::models::timestamp::Milliseconds;

pub struct AppConfig {
  pub time: Milliseconds,
  pub db:   Connection,
}
