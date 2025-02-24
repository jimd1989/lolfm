use crate::models::song::Song;
use crate::models::timestamp::Timestamp;

#[derive(Debug, PartialEq)]
pub enum LolfmEvent {
  DeleteBefore(Timestamp),
  RecordPlay(Timestamp, Song),
}
