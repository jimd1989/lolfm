use std::env::args;

#[derive(Debug)]
pub enum Cmd {
  Event,
  Init(String),
}

pub fn get_cmd() -> Result<Cmd, String> {
  let a: Vec<String> = args().collect();
  let ass: Vec<&str> = a.iter().map(|s| s.as_str()).collect();
  return parse_cmd(&ass);
}

fn parse_cmd(a: &Vec<&str>) -> Result<Cmd, String> {
  match a.as_slice() {
    [_, "event"]         => Ok(Cmd::Event),
    [_, "init", db_path] => Ok(Cmd::Init(db_path.to_string())),
    _                    => Err(format!("Bad cmd {:?}", a)),
  }
}
