function factorial_iter(mut x: u64): u64
begin
  let result = mut 1;

  while x > 2 do begin
    set result *= x;
    set x -= 1;
  end

  return result;
end

function factorial_rec(x: u64): u64
  if x < 2 then
    return x;
  else
    return x * factorial_rec(x - 1);
