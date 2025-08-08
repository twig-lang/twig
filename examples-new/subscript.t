subscript & sub_ref[x: i64]: i64 =
  &x;

subscript &mut sub_refmut[mut x: i64]: i64 =
  &mut x;

subscript sub_value[x: i64]: i64 =
  x;

function main = {
  let value = 100;

  {
    let ref = sub_ref[value];
  }

  {
    let refmut = sub_refmut[value];
  }

  {
    let val = sub_value[value];
    let valmut = mut sub_value[value];
  }
};
