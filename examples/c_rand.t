extern "C" function rand: i32;

function sum_of_rands: i32
begin
  let left = rand();
  let right = rand();
  return left + right;
end
