use sqlite::{Connection, State, Statement};
use std::iter;

use crate::models::er::Er;
use crate::models::country::Country;

pub fn get<'a>(db: &'a Connection)
-> Result<impl Iterator<Item = Result<Country, Er>> + 'a, Er> {
  let query = "
    SELECT countries.abbreviation,
           countries.name
      FROM countries; 
  ";
  let mut statement = db.prepare(query)?;
  Ok(iter::from_fn(move || {
    match statement.next() {
      Ok(State::Row)  => Some(to_country(&mut statement)),
      Ok(State::Done) => None,
      Err(ω)          => Some(Err(Er::from(ω)))
    }
  }))
}

fn to_country(statement: &mut Statement) -> Result<Country, Er> {
  let mut c      = Country::default();
  c.abbreviation = statement.read(0)?;
  c.name         = statement.read(1)?;
  Ok(c)
}
