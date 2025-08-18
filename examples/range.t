function while range(mut f from: u64, t to: u64): u64 = {
  while f < t {
    yield f;
    set f += 1;
  };
};

function example: u64 = {
  let sum = mut 0;

  while let i = range(from: 1, to: 100) {
    set sum += i;
  };

  sum
};
