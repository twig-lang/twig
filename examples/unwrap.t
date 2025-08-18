type MaybeInt = variant
  None,
  Some(i64);

subscript if &unwrap[&opt: MaybeInt]: i64 = {
  if match Some(x) = opt {
    yield x;
  }
};

function print(x: i64) = ();

function example = {
  let some = Some(0);
  let none = None;

  if let x = some unwrap { print(x); }

  if let x = none unwrap { print(x); }
};
