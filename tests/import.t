# Tests for imports.

- Can import another (empty) module.
  $ cat >Import1.tw <<EOF
  > import Import2;
  > EOF

  $ cat >Import2.tw <<EOF
  > {nothing going on here}
  > EOF

  $ twig check Import1.tw
