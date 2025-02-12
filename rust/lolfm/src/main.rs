mod repos {
  pub mod cmd;
  pub mod raw_cmus_event;
}
use repos::cmd::{Cmd, get_cmd};
use repos::raw_cmus_event::get_raw_cmus_event;

fn main() {
    let v = match get_cmd() {
      Ok(Cmd::Event)   => get_raw_cmus_event(),
      Ok(Cmd::Init(_)) => get_raw_cmus_event(),
      Err(e)           => Err(e),
    };
    println!("{:?}", v);
}
