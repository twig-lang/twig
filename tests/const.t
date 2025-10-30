# Tests for constant definitions.

- A constant unit literal.
  $ cat >unit_literal.tw <<EOF
  > const UNIT : () = ();
  > EOF

  $ twig check unit_literal.tw

- Cannot have a return expression in a constant.
  $ cat >fail_return.tw <<EOF
  > const FAIL : i32 = return 0;
  > EOF

  $ twig check --failing fail_return.tw
  failwith: unexpected return expression

- Fail to typecheck mismatching types.
  $ cat >fail_mismatch.tw <<EOF
  > const INT : () = 0;
  > EOF

  $ twig check --failing fail_mismatch.tw
  type mismatch: {integer} != ()
