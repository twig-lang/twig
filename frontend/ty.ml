module Primitive = struct
  type t =
    | Unit
    | Bool
    | U8
    | U16
    | U32
    | U64
    | I8
    | I16
    | I32
    | I64
    | F32
    | F64
    | Char
    | Str
  (* Built-in, base types *)

  let is_integer = function
    | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 -> true
    | _ -> false

  let is_signed = function
    | I8 | I16 | I32 | I64 | F32 | F64 -> true
    | _ -> false

  let is_real = function F32 | F64 -> true | _ -> false
  let equal : t -> t -> bool = ( == )
end

type t =
  | Primitive of Primitive.t
  | Integer
  | Real
  | Bottom
  | Named of Path.t
  | Pointer of Mode.mutability * t
  | Array of int * t
  | Slice of t
  | Tuple of t list
