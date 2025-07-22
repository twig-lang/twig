function fib_iter(x: u64): u64
begin
  let begin
    a = mut 1;
    b = mut 0;
    c = mut 0;
  end;

  while x > 0 do
    set begin
      c = a;
      a = a + b;
      b = c;

      x -= 1;
    end

  return a;
end

function fib_rec(x: u64): u64
  if x < 2 then
    return x;
  else
    return fib_rec(x - 1) + fib_rec(x - 2);
