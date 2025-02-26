use std::process::exit;

mod actions {
  pub mod dump_table;
  pub mod get_app_config;
  pub mod init_db;
  pub mod process_cmus_event;
}

mod helpers {
  pub mod sql_helpers;
}

mod models {
  pub mod app_config;
  pub mod cmd;
  pub mod cmus_event;
  pub mod cmus_status;
  pub mod cmus_tag;
  pub mod er;
  pub mod lolfm_event;
  pub mod played_song;
  pub mod song;
  pub mod table_name;
  pub mod timestamp;
}

mod repos {
  pub mod cmd_from_shell;
  pub mod cmus_events_from_db;
  pub mod cmus_events_to_db;
  pub mod cmus_tags_from_shell;
  pub mod db_connection;
  pub mod db_create_schema;
  pub mod lolfm_events_to_db;
  pub mod loved_songs_from_db;
  pub mod played_songs_from_db;
  pub mod songs_from_db;
  pub mod songs_to_db;
  pub mod system_time;
}

mod traits {
  pub mod cmus_decoder;
  pub mod cmus_encoder;
}

mod transformers {
  pub mod cmus_events_to_lolfm_events;
  pub mod cmus_tags_to_cmus_events;
  pub mod cmus_tags_to_songs;
}

use actions::dump_table;
use actions::get_app_config::get_app_config;
use actions::init_db;
use actions::process_cmus_event;
use models::cmd::Cmd;
use models::er::Er;
use repos::cmd_from_shell;

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
    Ok(Cmd::Dump(as_events, table, db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(dump_table::run(&config, table, as_events)?)
    }
    Ok(Cmd::Event(db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(process_cmus_event::run(&config)?)
    }
    Ok(Cmd::Init(db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(init_db::run(&config)?)
    }
    Err(ω) => Err(ω),
  }
}
