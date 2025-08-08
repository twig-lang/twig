function fib_iter(mut x: u64): u64 = {
  let {
    a = mut 1;
    b = mut 0;
    c = mut 0;
  };

  while x > 0 {
    set {
      c = a;
      a += b;
      b = c;
      x -= 1;
    }
  }

  a
};

function fib_rec(x: u64): u64 =
 if x < 2 { x }
 else { fib_rec(x - 1) + fib_rec(x - 2) };
