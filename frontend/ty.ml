type fixint_size = Ix8 | Ix16 | Ix32 | Ix64
type fixreal_size = Fx32 | Fx64

type t =
  | Unit
  | Bool
  | Char
  | Str
  | FixintSigned of fixint_size
  | FixintUnsigned of fixint_size
  | Fixreal of fixreal_size
  | Named of Path.t
  | Pointer of Mode.mutability * t
  | Array of int * t
  | Slice of t
  | Tuple of t list

let is_signed_int = function FixintSigned _ -> true | _ -> false
let is_unsigned_int = function FixintUnsigned _ -> true | _ -> false
let is_int t = is_signed_int t || is_unsigned_int t
let is_float = function Fixreal _ -> true | _ -> false

module Aliases = struct
  let unit = Unit
  let bool = Bool
  let char = Char
  let str = Str
  let i8 = FixintSigned Ix8
  let i16 = FixintSigned Ix16
  let i32 = FixintSigned Ix32
  let i64 = FixintSigned Ix64
  let u8 = FixintUnsigned Ix8
  let u16 = FixintUnsigned Ix16
  let u32 = FixintUnsigned Ix32
  let u64 = FixintUnsigned Ix64
  let f32 = Fixreal Fx32
  let f64 = Fixreal Fx64
end
