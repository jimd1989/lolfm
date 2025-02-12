use std::borrow::Cow;

#[derive(Debug)]
pub struct RawCmusEvent {
  pub status: Option<Cow<'static, str>>,
  pub artist: Option<Cow<'static, str>>,
  pub title: Option<Cow<'static, str>>,
  pub album: Option<Cow<'static, str>>,
  pub album_artist: Option<Cow<'static, str>>,
  pub genre: Option<Cow<'static, str>>,
  pub duration: usize,
  pub date: usize
}
