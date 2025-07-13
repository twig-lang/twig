subscript & sub_ref[x: i64]: i64
  yield &x;

subscript &mut sub_refmut[mut x: i64]: i64
  yield &mut x;

{ `subscript mut` is the same as this. }
subscript sub_value[x: i64]: i64
  yield x;

function main
begin
  let value = 100;

  begin
    let &ref = sub_ref[value];
  end

  begin
    let &mut refmut = sub_refmut[value];
  end

  begin
    let val = sub_value[value];
    let mut valmut = sub_value[value];
  end
end
