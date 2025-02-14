use sqlite::{Error, Statement, Value};

pub fn sql_int(s: &mut Statement, n: usize, α: i64) -> Result<(), Error> {
  s.bind((n, Value::Integer(α)))
}

pub fn sql_nullable_int(s: &mut Statement, n: usize, α: Option<i64>)
-> Result<(), Error> {
  sql_nullable(s, n, α, Value::Integer)
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
