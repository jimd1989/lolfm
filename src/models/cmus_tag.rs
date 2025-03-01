#[derive(Debug)]
pub struct CmusTag(pub Option<String>, pub Option<String>, pub Option<String>);

impl CmusTag {
  pub fn from_string(ω: String) -> Self {
    let mut s1  = ω.splitn(2, ' ');
    let s1_head = s1.next().map(|α| α.to_string());
    let s1_tail = s1.next();
    let mut s2  = s1_tail.map(|ω| ω.splitn(2, ' '));
    let s2_head = s2.as_mut().and_then(|α| α.next()).map(|α| α.to_string());
    let s2_tail = s2.as_mut().and_then(|α| α.next()).map(|α| α.to_string());
    Self(s1_head, s2_head, s2_tail)
  }
}
