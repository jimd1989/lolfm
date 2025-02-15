mod actions {
  pub mod get_app_config;
}
mod helpers {
  pub mod sql_helpers;
}
mod models {
  pub mod app_config;
  pub mod cmd;
  pub mod cmus_status;
  pub mod er;
  pub mod played_song;
  pub mod raw_cmus_event;
}
mod repos {
  pub mod cmd_from_shell;
  pub mod db_connection;
  pub mod db_init;
  pub mod raw_cmus_events_delete_from_db;
  pub mod raw_cmus_event_from_shell;
  pub mod raw_cmus_events_from_db;
  pub mod raw_cmus_event_to_db;
  pub mod system_time;
}
mod transformers {
  pub mod raw_cmus_events_to_played_songs;
}
use actions::get_app_config::get_app_config;
use models::cmd::Cmd;
use repos::cmd_from_shell::get_cmd_from_shell;
use repos::db_connection::connect_to_db;
use repos::db_init::init_db;
use repos::raw_cmus_events_delete_from_db::delete_raw_cmus_events_from_db;
use repos::raw_cmus_event_from_shell::get_raw_cmus_event_from_shell;
use repos::raw_cmus_event_to_db::write_raw_cmus_event_to_db;
use repos::raw_cmus_events_from_db::get_raw_cmus_events_from_db;

fn main() {
    match get_cmd_from_shell() {
      Ok(Cmd::Event(db_path)) => {
        let α      = get_app_config(&db_path).unwrap();
        let db     = connect_to_db(&db_path).unwrap();
        let event  = get_raw_cmus_event_from_shell(α.time_milliseconds).unwrap();
        let _z     = write_raw_cmus_event_to_db(&db, event).unwrap();
        let events = get_raw_cmus_events_from_db(&db).unwrap();
        let time   = events.last().unwrap().time_milliseconds.unwrap();
        //let _y     = delete_raw_cmus_events_from_db(&db, time).unwrap();
        println!("{:?}", events);
      },
      Ok(Cmd::Init(db_path))  => {
        let db = connect_to_db(&db_path).unwrap();
        println!("{:?}", init_db(&db))
      }
      Err(e)           => println!("{:?}", e),
    }
}
