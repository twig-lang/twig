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
  | TyProduct of ty list
  | TyPointer of ty
  | TyArray of int * ty
  | TySlice of ty
(* Types *)

type expr = EVariable of string
(* Expressions *)

type t = |

let from_ast a : t =
  ignore a;
  failwith "to be done"
