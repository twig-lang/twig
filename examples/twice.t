{ Defines a function that returns it's argument times 2. }

function i32 twice(i32 x) =
  2 * x;

function () main
begin
  let x = 10;
  let y = twice(x);
end
