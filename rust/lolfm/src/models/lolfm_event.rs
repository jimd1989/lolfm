use crate::models::song::Song;

#[derive(Debug, PartialEq)]
pub enum LolfmEvent {
  DeleteBefore(i64),
  RecordPlay(i64, Song),
}
