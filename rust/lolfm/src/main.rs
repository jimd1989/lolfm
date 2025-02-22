use std::process::exit;

mod actions {
  pub mod get_app_config;
  pub mod process_cmus_event;
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
  pub mod played_songs;
  pub mod raw_cmus_event;
}

mod repos {
  pub mod cmd_from_shell;
  pub mod db_connection;
  pub mod db_init;
  pub mod played_songs_to_db;
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
use actions::process_cmus_event::process_cmus_event;
use models::cmd::Cmd;
use models::er::Er;
use repos::cmd_from_shell;
use repos::db_init;

fn main() {
  match exec() {
    Err(ω) => {
      println!("{:?}", ω);
      exit(1);
    }
    Ok(_) => {
      exit(0);
    }
  }
}

fn exec() -> Result<(), Er> {
  match cmd_from_shell::get() {
    Ok(Cmd::Event(db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(process_cmus_event(&config)?)
    }
    Ok(Cmd::Init(db_path)) => {
      let config = get_app_config(&db_path)?;
      db_init::run(&config.db)
    }
    Err(ω) => Err(ω),
  }
}
