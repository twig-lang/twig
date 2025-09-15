type path_argument = PArgPath of path

and path =
  | PathAtom of string
  | PathCall of path * path_argument list
  | PathMember of path * string

type ty =
  (* TODO: Use something else here *)
  | TyPrimitive of string
  (* NOTE:
    In theory, the type of integer and real literals unify to a corresponding
    int/float type, OR they otherwise unify to i32/f32 respectively.

    This should allow typechecking:

      fn example: f64 = 0.0;
      { else it might fail to unify f32 and f64 }
  *)
  | TyInteger
  | TyReal
  | TyNamed of path
  | TyPointer of ty
  | TyArray of int * ty
  | TySlice of ty
  | TyTuple of ty list
  | TyUnknown of ty option ref
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
  | EVariable of ty * path
  | EIf of ty * expr * expr * expr
  | EWhen of expr * expr
(* Expressions *)

type env =
  | Env of {
      name : string;
      parent : env option;
      children : env list;
      bindings : (unit, unit) Hashtbl.t;
    }

(* matches two types, and returns an "unified" type. *)
let rec must l r =
  let must_prim l r =
    if not @@ String.equal l r then failwith "type mismatch"
  in

  let rec path_equal l r =
    match (l, r) with
    | PathAtom a, PathAtom b -> String.equal a b
    | PathMember (aa, ad), PathMember (ba, bd) ->
        path_equal aa ba && String.equal ad bd
    | _ -> false
  in

  match (l, r) with
  | TyPrimitive a, TyPrimitive b ->
      must_prim a b;
      l
  | TyUnknown l, r -> (
      match !l with
      | None ->
          l := Some r;
          r
      | Some t -> must t r)
  | r, TyUnknown l -> (
      match !l with
      | None ->
          l := Some r;
          r
      | Some t -> must t r)
  | TyPointer l, TyPointer r -> TyPointer (must l r)
  | TySlice l, TySlice r -> TySlice (must l r)
  (* NOTE: proper path checks require an env, cf:
    mod Mod = ( type t = (); );

    fn aliasing = (
      let mod Alias = Mod;

      { Mod.t == Alias.t }
      let absolute : Mod.t = ();
      let aliased : Alias.t = ();

      { thus both are valid }
      let valid : Mod.t = aliased;
      let also_valid : Alias.t = valid;

      ()
    );
  *)
  | TyNamed nl, TyNamed nr ->
      if path_equal nl nr then l else failwith "type mismatch"
  | TyTuple tl, TyTuple tr -> TyTuple (List.map2 must tl tr)
  | _ -> failwith "type mismatch"

(* returns a pair of type, annotated expr *)
let rec infer a : ty * expr =
  match a with
  | Ast.ExprUnit -> (TyPrimitive "unit", EUnit)
  | Ast.ExprBool b -> (TyPrimitive "bool", EBool b)
  | Ast.ExprInteger n -> (TyInteger, EInt n)
  | Ast.ExprReal f -> (TyReal, EReal f)
  | Ast.ExprIf (c, t, f) ->
      let tc, c = infer c in
      let tt, t = infer t in
      let tf, f = infer f in

      let _ = must tc (TyPrimitive "bool") in
      let ti = must tt tf in

      (ti, EIf (ti, c, t, f))
  | Ast.ExprWhen (c, b) ->
      let tc, c = infer c in
      let tb, b = infer b in

      let _ = must tc (TyPrimitive "bool") in
      let _ = must tb (TyPrimitive "unit") in

      (tb, EWhen (c, b))
  | _ -> failwith "uh oh"

(* TODO: pass an env here *)
let of_ast_toplevel =
  let open Ast in
  function
  | TopImport _i -> failwith "cannot import"
  | TopWith _w -> failwith "also cannot import"
  | TopFnDefinition _f -> failwith "functions"
  | TopSubDefinition _s -> failwith "subscripts"
  | TopConstDefinition _c -> failwith "constants"
  | TopTypeAbstract _t -> failwith "abstract types"
  | TopTypeDefinition _t -> failwith "typedefs"
  | TopExtern _e -> failwith "externs"
  | TopModDefinition _m -> failwith "mods"
  | TopSigDefinition _s -> failwith "sigs"

(* TODO:
  Maybe pass through toplevels twice to try to implement
  hoisting? i.e. avoid needing to have declarations), cf:

    fn left = right(); { no need to declare right() }
    fn right = ();

  Then go through every function definition?
*)
let of_ast = List.map of_ast_toplevel
