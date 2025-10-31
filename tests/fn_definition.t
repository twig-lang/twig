# Tests for function definitions.

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

- Fail to check mismatching types.
  $ cat >fail_mismatching_types.tw <<EOF
  > fn fail -> () = 1;
  > EOF

  $ twig check --failing fail_mismatching_types.tw
  type mismatch: {integer} != ()

- Have 1 positional argument.
  $ cat >1_p_arg.tw <<EOF
  > fn arg(a: ()) = a;
  > EOF

  $ twig check 1_p_arg.tw

- Have 1 named argument.
  $ cat >1_n_arg.tw <<EOF
  > fn arg(; a: ()) = a;
  > EOF

  $ twig check 1_n_arg.tw

- Have a declaration and a definition.
  $ cat >decl_def.tw <<EOF
  > fn function -> i32;
  > fn function -> i32 = 1;
  > EOF

  $ twig check decl_def.tw

- Have a mismatching declaration and definition (by return type).
  $ cat >fail_mismatching_ty_decl_def.tw <<EOF
  > fn function;
  > fn function -> i32 = 1;
  > EOF

  $ twig check --failing fail_mismatching_ty_decl_def.tw
  twig: internal error, uncaught exception:
        Failure("twig check : failed")
        
  [125]

- Have a mismatching declaration and definition (by argument list)
  $ cat >fail_mismatching_al_decl_def.tw <<EOF
  > fn function(arg : ());
  > fn function = ();
  > EOF

  $ twig check --failing fail_mismatching_al_decl_def.tw
  twig: internal error, uncaught exception:
        Failure("twig check : failed")
        
  [125]

- Cannot yield in a function.
  $ cat >fail_cannot_yield.tw <<EOF
  > fn fail = yield 0;
  > EOF

  $ twig check --failing fail_cannot_yield.tw
  failwith: unexpected yield expression
