use crate::models::song::Song;
use crate::models::timestamp::{Milliseconds, Seconds};

#[derive(Debug, PartialEq)]
pub enum LolfmEvent {
  DeleteBefore(Milliseconds),
  RecordPlay(Seconds, Song),
}
