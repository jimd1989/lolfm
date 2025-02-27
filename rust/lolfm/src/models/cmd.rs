use crate::models::table_name::TableName;

#[derive(Debug)]
pub enum Cmd {
  Event(String),
  Dump(bool, TableName, String),
  Love(i64, String),
  Unlove(i64, String),
  Init(String),
}
