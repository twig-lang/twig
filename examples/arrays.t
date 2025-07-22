with import Std::(Iterator, Memory);

{ Arrays of data can be defined. }

type Int23 = [23]i32;
type Int32 = [32]i32;

{ Arrays can have a length only known at runtime.
  these are a *different* type from above, known as a "slice type".

  As a few notes:
  - While [N]T is represented as a pointer, []T is a "fat pointer",
    as if equivalent to (:length uptr, :data *T).

  - Casting from [N]T to []T is automatic on arguments,
    and defined on manual casts, as in:
      take_a_slice(&my_array);       { automatic }
      let slice = my_array : []bool; { manual }

  - Indexing is done with the at[] subscript. There is no built-in
    syntax for array indexing. }

function sum_all_in(&input: []i32): i32
begin
  let result = mut 0;

  { assume there is one such subscript }
  let while i = input.each[] in
    set result += 1;

  return result;
end

function populate(&mut output: []i32)
begin
  let while begin
    value = output.each_mut[];
    index = iota[];
  end in
    set value = index;
end

function example
begin
  let n_i23 = mut unsafe Uninit!(Int23)::f();
  let n_i32 = mut unsafe Uninit!(Int32)::f();

  populate(&mut n_i23);
  populate(&mut n_i32);

  let sum = sum_all_in(&n_i23) + sum_all_in(&n_i32);

  let values = [1, 2, 4, 8, 16, 32, 64];
  set sum += sum_all_in(&values);

  { TODO: do something with sum. }
end
