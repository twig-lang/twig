function factorial_iter(mut x: u64): u64 = {
  let result = mut 1;

  while x > 2 {
    set result *= x;
    set x -= 1;
  }

  result
};

function factorial_rec(x: u64): u64 =
 if x < 2 { x }
 else { x * factorial_rec(x - 1) };
