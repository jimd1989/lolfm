use crate::models::table_name::TableName;

#[derive(Debug)]
pub enum Cmd {
  Dump(bool, TableName, String),
  Event(String),
  Init(String),
}
