use crate::models::table_name_dump::DumpTableName;
use crate::models::table_name_read::ReadTableName;

#[derive(Debug)]
pub enum Cmd {
  Event(String),
  Dump(bool, DumpTableName, String),
  Read(ReadTableName, String),
  Love(i64, String),
  Unlove(i64, String),
  Init(String),
}
