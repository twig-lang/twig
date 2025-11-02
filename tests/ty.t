# Tests for type definitions.

- Can define a type alias.
  $ cat >ty_alias.tw <<EOF
  > type integer = i32;
  > EOF

  $ twig check ty_alias.tw

- Can use a type alias.
  $ cat >use_alias.tw <<EOF
  > const KNOWN : i32 = 0;
  > type alias = i32;
  > const ALIASED : alias = KNOWN;
  > EOF

  $ twig check use_alias.tw
