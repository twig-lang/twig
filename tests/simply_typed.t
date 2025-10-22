This should test type checking and inference on simply typed programs.

- Typecheck a function returning unit.

  $ cat >function_unit.tw <<EOF
  > fn unit = ();
  > EOF

  $ twig check function_unit.tw

- Same as above, but make the signature explicit.
  $ cat >function_unit2.tw <<EOF
  > fn unit -> () = ();
  > EOF

  $ twig check function_unit2.tw

- Typecheck a function returning an int.
  $ cat >function_int.tw <<EOF
  > fn int -> i32 = 1;
  > EOF

  $ twig check function_int.tw

- Typecheck a block expression returning unit.
  $ cat >block.tw <<EOF
  > fn block = (
  >   ();
  >   ()
  > );
  > EOF

  $ twig check block.tw

- Typecheck a block expression returning an int.
  $ cat >block_int.tw <<EOF
  > fn block_int -> i32 = (
  >   ();
  >   1
  > );
  > EOF

  $ twig check block_int.tw

- Typecheck a constant integer.
  $ cat >const.tw <<EOF
  > const TWO: i32 = 2;
  > EOF

  $ twig check const.tw

- Typecheck a function call.
  $ cat >call.tw <<EOF
  > fn unit (left: ()) -> () = left;
  > fn example -> () =
  >   unit(());
  > EOF

  $ twig check call.tw

- Fail to typecheck a mismatched return type.
  $ cat >call_fail_return.tw <<EOF
  > fn unit = ();
  > fn num -> i32 = unit();
  > EOF

  $ twig check --failing call_fail_return.tw
  type mismatch: () != i32

- Typecheck a more complex example.
  $ cat >six.tw <<EOF
  > const ONE : i32 = 1;
  > const TWO : i32 = ONE + ONE;
  > fn three -> i32 =
  >   ONE + TWO;
  > fn three_times(x: i32) -> i32 =
  >   three() * x;
  > fn six -> i32 = three_times(): 2;
  > EOF

# To be done: Message sends.
#  $ twig check six.tw
