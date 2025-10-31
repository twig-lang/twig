# Tests for subscript definitions

- Can define one subscript.
  $ cat >sub.tw <<EOF
  > sub s = yield ();
  > EOF

  $ twig check sub.tw

- Subscripts cannot yield more than once.
  $ cat >fail_sub_2.tw <<EOF
  > sub s = (
  >  yield ();
  >  yield ()
  > );
  > EOF

  $ twig check --failing fail_sub_2.tw
  failwith: != 1 yields in subscript

- Subscripts cannot yield 0 times.
  $ cat >fail_0.tw <<EOF
  > sub zero = ();
  > EOF

  $ twig check --failing fail_0.tw
  failwith: != 1 yields in subscript

- A subscript with an argument.
  $ cat >sub_1arg.tw <<EOF
  > sub one[arg: i32] -> i32 = yield arg;
  > EOF

  $ twig check sub_1arg.tw

- A subscript yielding a reference.
  $ cat >sub_yield_ref.tw <<EOF
  > sub& ref[arg: i32] -> i32 = yield& arg;
  > EOF

  $ twig check sub_yield_ref.tw

- A subscript yielding a referenced argument.
  $ cat >sub_yield_ref_arg.tw <<EOF
  > sub& ra[&arg: i32] -> i32 = yield& arg;
  > EOF

  $ twig check sub_yield_ref_arg.tw

- Call a subscript.
  $ cat >call_sub.tw <<EOF
  > sub value -> i32 = yield 1;
  > fn val -> i32 = (
  >  let one : i32 = value[];
  >  2
  > );
  > EOF

  $ twig check call_sub.tw
