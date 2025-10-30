# Tests for expressions.

- Typecheck the unit literal.
  $ cat >tc_unit.tw <<EOF
  > const UNIT : () = ();
  > EOF

  $ twig check tc_unit.tw
