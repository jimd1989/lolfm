mod repos {
  pub mod cmd;
  pub mod init_db;
  pub mod raw_cmus_event;
  pub mod sqlite_connection;
}
use repos::cmd::{Cmd, get_cmd};
use repos::init_db::init_db;
use repos::raw_cmus_event::get_raw_cmus_event;
use repos::sqlite_connection::connect_to_sqlite;

fn main() {
    match get_cmd() {
      Ok(Cmd::Event(_))   => println!("{:?}", get_raw_cmus_event()),
      Ok(Cmd::Init(db_path)) => {
        let db = connect_to_sqlite(&db_path).unwrap();
        println!("{:?}", init_db(&db))
      }
      Err(e)           => println!("{:?}", e),
    }
}
