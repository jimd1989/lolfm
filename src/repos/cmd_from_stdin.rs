use std::env::args;

use crate::models::cmd::Cmd;
use crate::models::er::Er;
use crate::models::table_name_dump::DumpTableName;
use crate::models::table_name_read::ReadTableName;

pub fn get() -> Result<Cmd, Er> {
  let α: Vec<String> = args().collect();
  let αα: Vec<&str> = α.iter().map(|ω| ω.as_str()).collect();
  parse_cmd(&αα)
}

fn parse_cmd(α: &Vec<&str>) -> Result<Cmd, Er> {
  match α.as_slice() {
    [_, "event", db_path] => Ok(Cmd::Event(db_path.to_string())),
    [_, "dump", "-e", table, db_path] => {
      let name = DumpTableName::from_string(table)?;
      Ok(Cmd::Dump(true, name, db_path.to_string()))
    }
    [_, "dump", table, db_path] => {
      let name = DumpTableName::from_string(table)?;
      Ok(Cmd::Dump(false, name, db_path.to_string()))
    }
    [_, "read", table, db_path] => {
      let name = ReadTableName::from_string(table)?;
      Ok(Cmd::Read(name, db_path.to_string()))
    }
    [_, "love", id, db_path] => {
      let ω = id.parse::<i64>()?;
      Ok(Cmd::Love(ω, db_path.to_string()))
    }
    [_, "unlove", id, db_path] => {
      let ω = id.parse::<i64>()?;
      Ok(Cmd::Unlove(ω, db_path.to_string()))
    }
    [_, "init", db_path]  => Ok(Cmd::Init(db_path.to_string())),
    _ => Err(format!("Bad cmd {:?}", α).into()),
  }
}
