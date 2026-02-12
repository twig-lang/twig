This should test type checking and inference on simply typed programs.

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

- Typecheck let expressions.
  $ cat >let.tw <<EOF
  > const ONE : i32 = 1;
  > fn f_let -> i32 = (
  >   let one = ONE;
  >   one
  > );
  > EOF

  $ twig check let.tw

- Typecheck a failing example.
  $ cat >let_fail.tw <<EOF
  > fn f_let_fail -> i32 = (
  >   let float = 1.0;
  >   float
  > );
  > EOF

  $ twig check --failing let_fail.tw
  type mismatch: {real} != i32

- Let expressions with projections.
  $ cat >let_project.tw <<EOF
  > fn f -> i32 = (
  >   let one = 1;
  >   let also_one = &one;
  >   let two = mut 2;
  >   let also_two = &mut two;
  >   let again_two = &two;
  >   one
  > );
  > EOF

  $ twig check let_project.tw

- Typecheck loop and while expressions.
  $ cat >repeats.tw <<EOF
  > const TRUE : bool = true;
  > fn f_loop = loop ();
  > fn f_while -> i32 = (
  >   while TRUE do ();
  >   0
  > );
  > EOF

  $ twig check repeats.tw

- Typecheck break expressions
  $ cat >breaks.tw <<EOF
  > fn b_n_n -> i32 =
  >   label named -> _ do
  >     loop
  >       break named with 0;
  > EOF

  $ twig check breaks.tw

- Typecheck label arguments.
  $ cat >label_arg.tw <<EOF
  > fn brk(label exit : ()) = break exit;
  > fn lab = label l do brk(label exit: l);
  > EOF

  $ twig check label_arg.tw

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
