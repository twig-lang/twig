{ this subscript yields all numbers in a range. }
subscript while range[from: u64, to: u64]: u64
begin
  let value = mut from;

  while value < to do begin
    yield value;
    set value += 1;
  end
end

function example: i64
begin
  let sum = mut 0;

  let while i = range(1, 100) in
    set sum += i;

  return sum;
end
