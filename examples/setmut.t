{ Set a mutable reference to a value. }
function setmut(&mut ref: u64, value: u64)
  set ref = value;

function main
begin
  let mut ref = 100;
  let value = 50;

  setmut(&mut ref, value);
end
