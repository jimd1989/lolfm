use std::process::{Command, exit};

pub fn get_cmus_event() -> Vec<String> {
  return get_lines();
}

fn get_lines() -> Vec<String> {
  let cmd = "cmus-remote";
  let err = "is cmus running?";
  let run = Command::new(cmd).arg("-Q").output().expect(err);
  let res = String::from_utf8_lossy(&run.stdout);
  return res.lines().map(|l| l.to_string()).collect();
}
