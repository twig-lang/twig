import Std.Option;

type int = i64;

type also_int = (i64);

type unit = ();

type MaybeInt = Std.Option(int).t;

type IntAndUnit = record
  i: int,
  u: unit;

type AlsoIntAndUnit = (int, unit)

type Bit = enum
  Set,
  Clear;

type Bitcast = union
  i: i64,
  f: f64;

{ variants are a pair of an enum (a discriminant) and an union of records. }
type IntOrUnit = variant
  Int(int),
  Unit(unit);
