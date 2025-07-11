{ Defines a function that returns it's argument times 2. }

function twice(x: i32): i32 =
  2 * x;

function main
begin
  let x = 10;
  let y = twice(x);
end
