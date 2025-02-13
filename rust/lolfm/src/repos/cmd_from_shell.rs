use std::env::args;

use crate::models::cmd::Cmd;

pub fn get_cmd_from_shell() -> Result<Cmd, String> {
  let α: Vec<String> = args().collect();
  let αα: Vec<&str> = α.iter().map(|ω| ω.as_str()).collect();
  parse_cmd(&αα)
}

fn parse_cmd(α: &Vec<&str>) -> Result<Cmd, String> {
  match α.as_slice() {
    [_, "event", db_path] => Ok(Cmd::Event(db_path.to_string())),
    [_, "init", db_path]  => Ok(Cmd::Init(db_path.to_string())),
    _                     => Err(format!("Bad cmd {:?}", α)),
  }
}
