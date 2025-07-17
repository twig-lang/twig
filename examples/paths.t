{ Nonsense import paths. }

{ The parser distinguishes between:
  - Left `::` Right for "import paths",
  - Left `.` Right for "path members",
    which can include other paths. }

with import Things::MoreThings.(
  { Brings MoreThings.left into this namespace. }
  left,

  { And MoreThings.Utils.Right }
  Utils.right,

  Extras.(
    center,  { And so on. }
    And.top
  )
);
