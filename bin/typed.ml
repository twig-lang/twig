type path_argument = PArgPath of path

and path =
  | PathAtom of string
  | PathCall of path * path_argument list
  | PathMember of path * string

type ty_primitive =
  | T_unit
  | T_bool
  | T_u8
  | T_u16
  | T_u32
  | T_u64
  | T_i8
  | T_i16
  | T_i32
  | T_i64
  | T_f32
  | T_f64
(* Built-in, base types *)

type ty =
  | TyPrimitive of ty_primitive
  | TyNamed of path
  | TyPointer of ty
  | TyArray of int * ty
  | TySlice of ty
  | TyTuple of ty list
(* Types *)

type expr = EVariable of path
(* Expressions *)

type t = |

let from_ast a : t =
  ignore a;
  failwith "to be done"
