function fib_iter(x: u64): u64
begin
  let begin
    mut a = 1;
    mut b = 0;
  end;

  while x > 0 do
    set begin
      a = a + b;
      b = a;
      x -= 1;
    end

  return a;
end

function fib_rec(x: u64): u64
  if x < 2 then
    return x;
  else
    return fib_rec(x - 1) + fib_rec(x - 2);
