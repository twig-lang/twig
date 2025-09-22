module type STAGE = sig
  (* This stage's type variable. *)
  type ty_variable_proof

  (* The type of variable names. *)
  type variable_name
end

module StringMap = Map.Make (String)

module TreeS (S : STAGE) = struct
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

  type ty_variable = S.ty_variable_proof * ty option ref

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
    | TyNamed of S.variable_name
    | TyPointer of ty
    | TyArray of int * ty
    | TySlice of ty
    | TyTuple of ty list
    | TyUnknown of ty_variable
  (* Types *)

  type expr =
    | EUnit
    | EInt of int
    | EReal of float
    | EBool of bool
    | EString of string
    | EChar of Uchar.t
    | ETuple of ty * expr list
    | EList of ty * expr list
    | EVariable of ty * S.variable_name
    | EIf of ty * expr * expr * expr
    | EWhen of expr * expr
    | EReturn of expr
    (* returned type, non-returned values (of type ()) and returned value *)
    | EBlock of ty * expr list * expr
  (* Expressions *)

  type fn_definition = { return : ty; value : expr }
  type fn_signature = { return : ty }
  type ty_definition = { ty : ty }
  type const_definition = { ty : ty; value : expr }
  type const_signature = { ty : ty }

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
end

module type CVT = sig
  module From : STAGE
  module To : STAGE

  val cvt : TreeS(From).m -> TreeS(To).m
end
