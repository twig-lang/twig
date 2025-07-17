type Bit = enum
  Set, Clear;

function not(x: Bit): Bit
  match x begin
    { NOTE: Unlike in C, cases have no implicit fallthrough. }

    case Set:
      return Clear;

    case Clear:
      return Set;
  end

function and(l: Bit, r: Bit): Bit
  if l = Set && r = Set then
    return Set;
  else
    return Clear;

function or(l: Bit, r: Bit): Bit
  if l = Set || r = Set then
    return Set;
  else
    return Clear;
