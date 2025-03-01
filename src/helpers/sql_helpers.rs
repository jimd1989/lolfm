use sqlite::{Error, State, Statement, Value};

pub fn sql_int(s: &mut Statement, n: usize, α: i64) -> Result<(), Error> {
  s.bind((n, Value::Integer(α)))
}

pub fn sql_string(s: &mut Statement, n: usize, α: String) -> Result<(), Error> {
  s.bind((n, Value::String(α)))
}

pub fn sql_nullable_string(s: &mut Statement, n: usize, α: Option<String>)
  -> Result<(), Error> {
  sql_nullable(s, n, α, Value::String)
}

fn sql_nullable<A, F>(s: &mut Statement, n: usize, α: Option<A>, f: F)
-> Result<(), Error> where F: Fn(A) -> Value {
  match α {
    Some(ω) => s.bind((n, f(ω))),
    None    => s.bind((n, Value::Null)),
  }
}

pub fn sql_execute_void(ω: &mut Statement) -> Result<(), Error> {
  let e = Error { code: None, message: Some("got rows from void".to_string()) };
  match ω.next() {
    Ok(State::Done) => Ok(()),
    Err(α)          => Err(α),
    _               => Err(e),
  }
}
