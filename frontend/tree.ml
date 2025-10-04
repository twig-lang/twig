module StringMap = Map.Make (String)

type primitive_type =
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

type ty_variable = ty option ref

and ty =
  | TyPrimitive of primitive_type
  (* NOTE:
    In theory, the type of integer and real literals unify to a corresponding
    int/float type, OR they otherwise unify to i32/f32 respectively.

    This should allow typechecking:

      fn example -> f64 = 0.0;
      { else it might fail to unify f32 and f64 }
  *)
  | TyInteger
  | TyReal
  (* The ! type. Unifies with any other type. *)
  | TyBottom
  | TyNamed of Path.t
  | TyPointer of ty
  | TyArray of int * ty
  | TySlice of ty
  | TyTuple of ty list
  | TyUnknown of ty_variable
(* Types *)

let rec fmt_ty f =
  let open Printf in
  function
  | TyPrimitive p ->
      fprintf f
        (match p with
        | T_unit -> "()"
        | T_bool -> "bool"
        | T_u8 -> "u8"
        | T_u16 -> "u16"
        | T_u32 -> "u32"
        | T_u64 -> "u64"
        | T_i8 -> "i8"
        | T_i16 -> "i16"
        | T_i32 -> "i32"
        | T_i64 -> "i64"
        | T_f32 -> "f32"
        | T_f64 -> "f64")
  | TyReal -> fprintf f "{real}"
  | TyInteger -> fprintf f "{integer}"
  | TyBottom -> fprintf f "!"
  | TyNamed name -> fprintf f "%a" Path.fmt name
  | TyPointer p -> fprintf f "* %a" fmt_ty p
  | TyArray (n, i) -> fprintf f "[%d] %a" n fmt_ty i
  | TySlice i -> fprintf f "[] %a" fmt_ty i
  | TyUnknown r -> (
      match !r with
      | Some t -> fprintf f "?<%a>" fmt_ty t
      | None -> fprintf f "?")
  | TyTuple is ->
      let rec f' c = function
        | x :: [] -> fprintf c "%a" fmt_ty x
        | x :: xs ->
            fprintf c "%a, " fmt_ty x;
            f' c xs
        | [] -> ()
      in
      fprintf f "(%a)" f' is

type positional_param = Value of Mode.t * string * ty | Label of string * ty

type named_param =
  | Value of Mode.t * string * ty
  | Label of string * ty
  | Key of Mode.t * string * ty * expr

and positional_arg = Value of Mode.t * expr
and named_arg = NamedValue of Mode.t * string * expr

and expr =
  | EUnit
  | EInt of int
  | EReal of float
  | EBool of bool
  | EString of string
  | EChar of Uchar.t
  | ETuple of ty * expr list
  | EList of ty * expr list
  | EVariable of ty * Path.t
  | EIf of ty * expr * expr * expr
  | EWhen of expr * expr
  | EReturn of expr
  (* returned type, non-returned values (of type ()) and returned value *)
  | EBlock of ty * expr list * expr
  (* returned type, function, positional, named *)
  | ECall of expr * positional_arg list * named_arg list

(* Expressions *)

type param_list = positional_param list * named_param list
type ty_definition = { ty : ty }
type fn_signature = { return : ty; arguments : param_list }
type const_signature = { ty : ty }
type fn_definition = { s : fn_signature; value : expr }
type const_definition = { s : const_signature; value : expr }

type m = {
  parent : m option;
  fn_definitions : fn_definition Env.t;
  const_definitions : const_definition Env.t;
  types : ty_definition Env.t;
  modules : m Env.t;
  fn_signatures : fn_signature Env.t;
  const_signatures : const_signature Env.t;
}

let empty =
  Env.
    {
      parent = None;
      fn_definitions = empty;
      const_definitions = empty;
      types = empty;
      modules = empty;
      fn_signatures = empty;
      const_signatures = empty;
    }

let rec get_rec_module p m =
  match p with
  | Path.Atom a -> (a, m)
  | Path.Member (p, a) ->
      let a', m = get_rec_module p m in
      let m = Env.read a' m.modules in
      (a, m)
  | _ -> failwith "unsupported path!"

let get_fnsig p m =
  let a, m = get_rec_module p m in
  Env.read a m.fn_signatures

let get_ksig p m =
  let a, m = get_rec_module p m in
  Env.read a m.const_signatures

let get_ty p m =
  let a, m = get_rec_module p m in
  Env.read a m.types
