type Bit = enum
  Set, Clear;

function not(x: Bit): Bit =
 match x {
  case Set: Clear;
  case Clear: Set;
 };

function and(l: Bit, r: Bit): Bit =
 if l = Set && r = Set { Set } else { Clear };

function or(l: Bit, r: Bit): Bit =
 if l = Set || r = Set { Set } else { Clear };
