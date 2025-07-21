function i32_of_f32(x: f32): i32 =
  { To cast from one type to another, use the ':' "operator":
      expression : type
  }
  x : i32;

{ : has a higher precedence than any other operator, so this: }
function f(x: f32): i32 =
  x : i32 + 6;
  { parses as (x : i32) + 6 }
