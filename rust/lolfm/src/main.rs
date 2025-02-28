use std::process::exit;

mod actions {
  pub mod dump_table;
  pub mod get_app_config;
  pub mod init_db;
  pub mod love_song;
  pub mod process_cmus_event;
  pub mod unlove_song;
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
  pub mod loved_song;
  pub mod played_song;
  pub mod song;
  pub mod table_name;
  pub mod timestamp;
}

mod repos {
  pub mod cmd_from_shell;
  pub mod cmus_events_from_db;
  pub mod cmus_events_to_db;
  pub mod db_connection;
  pub mod db_create_schema;
  pub mod lines_from_cmus_remote;
  pub mod lolfm_events_to_db;
  pub mod love_song_in_db;
  pub mod loved_song_delete_from_db;
  pub mod loved_songs_from_db;
  pub mod played_songs_from_db;
  pub mod songs_from_db;
  pub mod songs_to_db;
  pub mod system_time;
}

mod traits {
  pub mod cmus_event_decoder;
  pub mod cmus_event_encoder;
  pub mod row_encoder;
}

mod transformers {
  pub mod cmus_events_to_lolfm_events;
  pub mod cmus_tags_to_cmus_events;
  pub mod cmus_tags_to_songs;
  pub mod lines_to_cmus_tags;
}

use actions::dump_table;
use actions::get_app_config::get_app_config;
use actions::init_db;
use actions::love_song;
use actions::process_cmus_event;
use actions::unlove_song;
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
    Ok(Cmd::Event(db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(process_cmus_event::run(&config)?)
    }
    Ok(Cmd::Dump(as_events, table, db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(dump_table::run(&config, table, as_events)?)
    }
    Ok(Cmd::Love(id, db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(love_song::run(&config, id)?)
    }
    Ok(Cmd::Unlove(id, db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(unlove_song::run(&config, id)?)
    }
    Ok(Cmd::Init(db_path)) => {
      let config = get_app_config(&db_path)?;
      Ok(init_db::run(&config)?)
    }
    Err(ω) => Err(ω),
  }
}
