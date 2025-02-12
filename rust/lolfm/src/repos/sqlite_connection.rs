use sqlite::Connection;

pub fn connect_to_sqlite(db_path: &String) -> Result<Connection, String> {
  sqlite::open(db_path).map_err(|e| e.to_string())
}
