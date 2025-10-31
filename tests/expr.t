# Tests for expressions.

- Typecheck the unit literal.
  $ cat >tc_unit.tw <<EOF
  > const UNIT : () = ();
  > EOF

  $ twig check tc_unit.tw

- Can set a mutable variable.
  $ cat >can_set_mut.tw <<EOF
  > fn u = (
  >  let x = mut 0;
  >  set x = 1
  > );
  > EOF

  $ twig check can_set_mut.tw

- Can not set an immutable variable.
  $ cat >fail_set_immut.tw <<EOF
  > fn u = (
  >  let x = 0;
  >  set x = 1
  > );
  > EOF

  $ twig check --failing fail_set_immut.tw
  failwith: lvalue is not mutable
