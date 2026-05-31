use sqlite::{Connection, State, Statement};
use std::iter;

use crate::models::er::Er;
use crate::models::artist::Artist;

pub fn get<'a>(db: &'a Connection)
-> Result<impl Iterator<Item = Result<Artist, Er>> + 'a, Er> {
  let query = "
    SELECT artists.id,
           artists.name,
           countries.abbreviation AS country_abbreviation,
           countries.name AS country
      FROM artists
      JOIN countries
        ON artists.country = countries.id;
  ";
  let mut statement = db.prepare(query)?;
  Ok(iter::from_fn(move || {
    match statement.next() {
      Ok(State::Row)  => Some(to_artist(&mut statement)),
      Ok(State::Done) => None,
      Err(ω)          => Some(Err(Er::from(ω)))
    }
  }))
}

fn to_artist(statement: &mut Statement) -> Result<Artist, Er> {
  let mut c              = Artist::default();
  c.id                   = statement.read(0)?;
  c.name                 = statement.read(1)?;
  c.country_abbreviation = statement.read(2)?;
  c.country              = statement.read(3)?;
  Ok(c)
}
