use sqlite::Connection;

pub struct AppConfig {
  pub time_milliseconds:  i64,
  pub db:                 Connection,
}
