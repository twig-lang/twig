# Tests for imports.

- Can import another (empty) module.
  $ cat >ImportEmpty1.tw <<EOF
  > import Import2;
  > EOF

  $ cat >ImportEmpty2.tw <<EOF
  > {nothing going on here}
  > EOF

  $ twig check ImportEmpty1.tw

- Can import contents from another module.
  $ cat >ImportContents1.tw <<EOF
  > import ImportContents2;
  > const MY_NUMBER : i32 = ImportContents2::THEIR_NUMBER;
  > EOF

  $ cat >ImportContents2.tw <<EOF
  > const THEIR_NUMBER : i32 = 2;
  > EOF

#  $ twig check ImportContents1.tw
