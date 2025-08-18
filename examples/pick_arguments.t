function pick(
  condition: bool,
  t taken     = 1: i64,
  f not_taken = 0: i64
): i64 =
 if condition { t } else { f };

function id(x value: i64): i64 = x;

function main = {
  pick(false);
  pick(true);
  pick(true, taken: 2);
  pick(false, not_taken: 3);

  pick(true,
    taken: 4,
    not_taken: 5);

  id(value: 5);
};
