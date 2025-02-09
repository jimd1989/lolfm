mod repos {
  pub mod cmd;
  pub mod cmus_event;
}
use repos::cmd::{Cmd, get_cmd};
use repos::cmus_event::get_cmus_event;

fn main() {
    let v = match get_cmd() {
      Ok(Cmd::Event)   => get_cmus_event(),
      Ok(Cmd::Init(_)) => get_cmus_event(),
      Err(e)           => [e].to_vec(),
    };
    println!("{:?}", v);
}
