open Text

type path_argument = PArgPath of path

and path =
  | PathAtom of string
  | PathCall of path * path_argument list
  | PathMember of path * string

module Mode = struct
  type t = bool * bool (* mutable? , reference? *)

  let create ?(mut = false) ?(reference = false) () = (mut, reference)
  let is_mutable (m : t) = fst m
  let is_reference (m : t) = snd m
end

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

(* ty_primitive -> is_signed option *)
let is_int_ty_primitive = function
  | T_u8 | T_u16 | T_u32 | T_u64 -> Some false
  | T_i8 | T_i16 | T_i32 | T_i64 -> Some true
  | _ -> None

(* ty_primitive -> is_f32 option *)
let is_real_ty_primitive = function
  | T_f32 -> Some true
  | T_f64 -> Some false
  | _ -> None

type ty =
  | TyPrimitive of ty_primitive
  (* NOTE:
    In theory, the type of integer and real literals unify to a corresponding
    int/float type, OR they otherwise unify to i32/f32 respectively.

    This should allow typechecking:

      fn example: f64 = 0.0;
      { else it might fail to unify f32 and f64 }
  *)
  | TyInteger
  | TyReal
  (* The ! type. Unifies with any other type. *)
  | TyBottom
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
  | EReturn of expr
  (* returned type, non-returned values (of type ()) and returned value *)
  | EBlock of ty * expr list * expr
(* Expressions *)

type fn_item = { name : string; return_type : ty; value : expr }
type ty_item = ty

type mod_item =
  | Function of { name : string; return_type : ty; value : expr }
  | Type of ty

type item =
  | FnItem of { name : string; return : ty; value : expr }
  | TyItem of ty
  | ModItem of mod_env

and mod_env = { parent : mod_env option; items : (string, item) Hashtbl.t }

let create_mod_env () = { parent = None; items = Hashtbl.create 0 }

let create_sub_mod_env parent =
  let env = create_mod_env () in
  { env with parent = Some parent }

let must_mod = function ModItem e -> e | _ -> failwith "expected a module!"

let rec lookup env = function
  | PathAtom atom -> Hashtbl.find env.items atom
  | PathMember (mem, item) ->
      let env = must_mod @@ lookup env mem in
      Hashtbl.find env.items item
  | _ -> failwith "unsupported path"

let rec translate_path = function
  | Ast.PathAtom atom -> PathAtom atom
  | Ast.PathMember (parent, child) -> PathMember (translate_path parent, child)
  | _ -> failwith "cannot translate path"

(* matches two types, and returns an "unified" type. *)
let rec must l r =
  let must_prim l r = if l <> r then failwith "type mismatch" in

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
  | TyBottom, _ -> r
  | _, TyBottom -> l
  | TyUnknown l, _ -> (
      match !l with
      | None ->
          l := Some r;
          r
      | Some t -> must t r)
  | _, TyUnknown l -> (
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

module StringMap = Map.Make (String)

type infer_env = {
  expected_return : ty;
  bindings : (ty * Mode.t * expr) StringMap.t;
}

(* returns a tuple of type, annotated expr, new env *)
let rec infer env =
  let rec infer_block env a = function
    | [] -> (env, TyPrimitive T_unit, a, EUnit)
    | t :: [] ->
        let tt, t, env = infer env t in
        (env, tt, List.rev a, t)
    | t :: ts ->
        let tt, t, env = infer env t in
        let _ = must tt (TyPrimitive T_unit) in
        infer_block env (t :: a) ts
  in

  function
  | Ast.ExprUnit -> (TyPrimitive T_unit, EUnit, env)
  | Ast.ExprBool b -> (TyPrimitive T_bool, EBool b, env)
  | Ast.ExprInteger n -> (TyInteger, EInt n, env)
  | Ast.ExprReal f -> (TyReal, EReal f, env)
  | Ast.ExprIf (c, t, f) ->
      let tc, c, env = infer env c in
      let tt, t, env = infer env t in
      let tf, f, env = infer env f in

      let _ = must tc (TyPrimitive T_bool) in
      let ti = must tt tf in

      (ti, EIf (ti, c, t, f), env)
  | Ast.ExprWhen (c, b) ->
      let tc, c, env = infer env c in
      let tb, b, env = infer env b in

      let _ = must tc (TyPrimitive T_bool) in
      let _ = must tb (TyPrimitive T_unit) in

      (tb, EWhen (c, b), env)
  | Ast.ExprReturn None -> (TyBottom, EReturn EUnit, env)
  | Ast.ExprReturn (Some v) ->
      let _, v, env = infer env v in
      (* TODO: match with the function's return type *)
      (TyBottom, EReturn v, env)
  | Ast.ExprBlock items ->
      let env, returned, units, value = infer_block env [] items in
      (returned, EBlock (returned, units, value), env)
  | Ast.ExprLet (pat, _ty, _mode, value) ->
      let env =
        match pat with
        | PatNamed (Ast.PathAtom name) ->
            let t, e, env = infer env value in
            let binds =
              StringMap.add name (t, Mode.create (), e) env.bindings
            in
            { env with bindings = binds }
        | _ -> failwith "unsupported pattern"
      in
      (TyPrimitive T_unit, EUnit, env)
  | Ast.ExprVariable path -> (
      match path with
      | Ast.PathAtom atom ->
          let ty, _, value = StringMap.find atom env.bindings in
          (ty, value, env)
      | _ -> failwith "unsupported path")
  | _ -> failwith "uh oh"

let must_item = function TyItem it -> it | _ -> failwith "expected a type!"

let translate_ast_type (menv : mod_env) = function
  | Ast.TyUnit -> TyPrimitive T_unit
  | Ast.TyNamed path -> must_item @@ lookup menv (translate_path path)
  | _ -> failwith "unknown type"

let of_ast_toplevel (top_env : mod_env) =
  let open Ast in
  function
  | TopImport _i -> failwith "cannot import"
  | TopWith _w -> failwith "also cannot import"
  | TopFnDefinition f ->
      let fn_value = f.value in
      let fn_returns = f.ty in
      let _fn_name = f.name in

      let returns = Option.value ~default:Ast.TyUnit fn_returns in

      let t = translate_ast_type top_env returns in

      let env = { expected_return = t; bindings = StringMap.empty } in

      let value = Option.value ~default:Ast.ExprUnit fn_value in
      let vt, v, _env = infer env value in

      let _ = (must vt, t) in

      let binding = FnItem { name = "fn_name"; return = vt; value = v } in

      Hashtbl.add top_env.items "fn_name" binding;

      top_env
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

let add_item env = Hashtbl.add env.items

let of_ast ast =
  let env = create_mod_env () in

  add_item env "i32" (TyItem (TyPrimitive T_i32));

  List.fold_left of_ast_toplevel env ast
