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
  | TyUnknown of ty ref
(* Types *)

type expr = EVariable of path
(* Expressions *)

type env =
  | Env of {
      name : string;
      parent : env option;
      children : env list;
      bindings : (unit, unit) Hashtbl.t;
    }



let from_ast_toplevel =
  let open Ast in
  function
  | TopImport _i -> failwith "cannot import"
  | TopWith _w -> ()
  | TopFnDefinition _f -> ()
  | TopSubDefinition _s -> ()
  | TopConstDefinition _c -> ()
  | TopTypeAbstract _t -> ()
  | TopTypeDefinition _t -> ()
  | TopExtern _e -> ()
  | TopModDefinition _m -> ()
  | TopSigDefinition _s -> ()

let from_ast_toplevels = List.map from_ast_toplevel
