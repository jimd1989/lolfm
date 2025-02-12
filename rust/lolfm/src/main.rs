mod models {
  pub mod cmd;
  pub mod raw_cmus_event;
}
mod repos {
  pub mod cmd_from_shell;
  pub mod init_db;
  pub mod raw_cmus_event_from_shell;
  pub mod sqlite_connection;
}
use models::cmd::Cmd;
use repos::cmd_from_shell::get_cmd_from_shell;
use repos::init_db::init_db;
use repos::raw_cmus_event_from_shell::get_raw_cmus_event_from_shell;
use repos::sqlite_connection::connect_to_sqlite;

fn main() {
    match get_cmd_from_shell() {
      Ok(Cmd::Event(_))   => println!("{:?}", get_raw_cmus_event_from_shell()),
      Ok(Cmd::Init(db_path)) => {
        let db = connect_to_sqlite(&db_path).unwrap();
        println!("{:?}", init_db(&db))
      }
      Err(e)           => println!("{:?}", e),
    }
}
