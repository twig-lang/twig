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

- Number literal types can "decay" into fixed-width versions.
  $ cat >num_decay.tw <<EOF
  > const I8: i8 = 0;
  > const I16: i16 = 0;
  > const I32: i32 = 0;
  > const I64: i64 = 0;
  > const U8: u8 = 0;
  > const U16: u16 = 0;
  > const U32: u32 = 0;
  > const U64: u64 = 0;
  > const F32: f32 = 0.0;
  > const F64: f64 = 0.0;
  > EOF

  $ twig check num_decay.tw

- Real number literals do not have an integer type.
  $ cat >num_real_not_int.tw <<EOF
  > const Real: i32 = 0.0;
  > EOF

  $ twig check --failing num_real_not_int.tw
  type mismatch: {real} != i32

- Integer literals do not have a float type.
  $ cat >num_int_not_real.tw <<EOF
  > const Int: f32 = 0;
  > EOF

  $ twig check --failing num_int_not_real.tw
  type mismatch: {integer} != f32

- Fail to typecheck mismatched parameter types.
  $ cat >fail_mismatch_parameters.tw <<EOF
  > fn id(x: f32) -> f32 = x;
  > fn fail -> f32 = id(42);
  > EOF

  $ twig check --failing fail_mismatch_parameters.tw
  type mismatch: f32 != {integer}

- Check with constants.
  $ cat >check_const.tw <<EOF
  > const ONE : i32 = 1;
  > fn one -> i32 = ONE;
  > EOF

  $ twig check check_const.tw

- Typecheck return expressions.
  $ cat >return.tw <<EOF
  > fn ret_one -> i32 = return 0;
  > EOF

  $ twig check return.tw

- Typecheck if expressions.
  $ cat >if.tw <<EOF
  > fn ret_when(cond: bool) -> i32 = if cond then 1 else 2;
  > EOF

  $ twig check if.tw

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
