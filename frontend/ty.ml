type primitive =
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
(* Built-in, base types *)

type 'v ext =
  | Enumeration of (string * 'v t list) list
  | Structure of (string * 'v t) list
  | Union of (string * 'v t) list

and 'variable t =
  | Primitive of primitive
  | Integer
  | Real
  | Bottom
  | Named of Path.t
  | Pointer of Mode.mutability * 'variable t
  | Array of int * 'variable t
  | Slice of 'variable t
  | Tuple of 'variable t list
  (* as defined by type definitions *)
  | Extension of 'variable ext
  (* the type variable itself *)
  | Variable of 'variable
(* Types *)
