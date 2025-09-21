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
