{ Pick one of two arguments based on a condition. }
function pick(
  condition: bool,
  t taken: i64     = 1,
  f not_taken: i64 = 0
): i64
  if condition then
    return t;
  else
    return f;

{ If no default value is given, a value must
  be passed on every call. }
function id(x value: i64): i64 = x;

function main
begin
  pick(false);               { 0 }
  pick(true);                { 1 }
  pick(true, taken: 2);      { 2 }
  pick(false, not_taken: 3); { 3 }
  pick(true,
    taken: 4,
    not_taken: 5);           { 4 }

  id(value: 5); { OK }
{ id();         { ERROR } }
end
