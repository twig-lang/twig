open Env

exception TypeMismatch of variable Ty.t * variable Ty.t

let fresh () = Variable (ref None)
let get (Variable var) = !var
let set (Variable var) x = var := Some x

(* Create a Tree from a list of toplevel statements *)
let tree_of_toplevels tops = List.fold_left Tree.add Tree.empty tops

(*( Perform type-checking and inference )*)
let mismatch l r = raise @@ TypeMismatch (l, r)

let unify_primitive l r =
  if Ty.equal_prim l r then Ty.Primitive l
  else mismatch (Ty.Primitive l) (Ty.Primitive r)

let resolve_named context = function
  | Ty.Named name ->
      let def = Tree.get_ty name context in
      def.ty
  | x -> x

(* "unify" two types, and return the unified version *)
let rec unify context (l : variable Ty.t) (r : variable Ty.t) =
  let l = resolve_named context l in
  let r = resolve_named context r in

  match (l, r) with
  (* Unify two primitives iff they are equal *)
  | Ty.Primitive l, Ty.Primitive r -> unify_primitive l r
  (* Bottom unifies with any other type *)
  | Ty.Bottom, _ -> r
  | _, Ty.Bottom -> l
  (* literal types unify with their fixed-width counterparts *)
  | Ty.Integer, Ty.Primitive p when Ty.is_primitive_integer p -> r
  | Ty.Primitive p, Ty.Integer when Ty.is_primitive_integer p -> l
  | Ty.Real, Ty.Primitive p when Ty.is_primitive_real p -> r
  | Ty.Primitive p, Ty.Real when Ty.is_primitive_real p -> l
  (* literal types are also equal to themselves *)
  | Ty.Integer, Ty.Integer -> l
  | Ty.Real, Ty.Real -> l
  (* Variables get populated if empty, unified if not *)
  | Ty.Variable l, _ -> (
      match get l with
      | Some t -> unify context t r
      | None ->
          set l r;
          r)
  | _, Ty.Variable r -> (
      match get r with
      | Some t -> unify context l t
      | None ->
          set r l;
          l)
  (* TODO: The rest of the cases. *)
  (* Any other case is a mismatch *)
  | _ -> mismatch l r

(* only perform the type check *)
let check context l r = ignore @@ unify context l r

let rec infer_block (env : Env.t) valued = function
  | x :: xs ->
      let env, _, tx = infer env x in
      check (context_of env) (Ty.Primitive Ty.Unit) tx;

      infer_block env valued xs
  | [] -> infer env valued

and check_arguments env (p, n) positional named =
  (* TODO: handle labels later on *)
  (* TODO: at least type check this *)
  List.iter2
    (fun param (Expr.AValue (am, av)) ->
      match param with
      | Expr.PPValue (m, _, t) ->
          let env, _, at = infer env av in
          ignore @@ Mode.project m am;
          check (context_of env) t at
      | Expr.PPLabel _ -> failwith "label parameters not supported yet")
    p positional;

  (* TODO: named arguments are not checked in order,
    maybe turn them from a list into an Env.t? *)
  List.iter2 (fun _param _arg -> ()) n named;

  ()

(* infer the type of an expression *)
and infer (env : Env.t) expr : Env.t * Mode.t * variable Ty.t =
  let literal_mode = Mode.create ~mut:Mode.Mutable () in

  let literal_ty ty = (env, literal_mode, ty) in

  match expr with
  | Expr.Unit -> literal_ty (Ty.Primitive Ty.Unit)
  | Expr.Int _ -> literal_ty Ty.Integer
  | Expr.Real _ -> literal_ty Ty.Real
  | Expr.Bool _ -> literal_ty (Ty.Primitive Ty.Bool)
  | Expr.Block (units, valued) -> infer_block env valued units
  | Expr.Call (Expr.Variable name, positional, named) ->
      (* TODO: support actual callable values *)
      let fn = Tree.get_fnsig name (context_of env) in
      check_arguments env fn.arguments positional named;
      literal_ty fn.return
  | Expr.Call _ -> failwith "unsupported callee"
  | Expr.Variable (Path.Atom name) -> (
      match find_variable env name with
      | Some (m, ty) -> (env, m, ty)
      | None ->
          let s = Tree.get_ksig (Path.Atom name) (context_of env) in
          literal_ty s.ty)
  | Expr.Variable _ -> failwith "unsupported variable path"
  | Expr.Return value -> (
      let exp = expect_of env in
      let env, m, tv = infer env value in

      (* forbid returning projected values *)
      if not Mode.(equal (unproject m) m) then
        raise @@ Mode.ProjectionFailure (Mode.unproject m, m);

      match exp.return with
      | Some ty ->
          check (context_of env) ty tv;
          literal_ty Ty.Bottom
      | None -> failwith "unexpected return expression")
  | Expr.If (condition, t, f) ->
      let env, _, tc = infer env condition in
      let env, _, tt = infer env t in
      let env, _, tf = infer env f in
      check (context_of env) (Ty.Primitive Ty.Bool) tc;
      literal_ty @@ unify (context_of env) tt tf
  | Expr.Let (name, mode, ty, value) ->
      let ty = Option.value ~default:(Ty.Variable (fresh ())) ty in
      let _, m, tv = infer env value in
      ignore @@ Mode.project mode m;
      check (context_of env) ty tv;
      let env = add_var env name mode tv in
      (env, Mode.create (), Ty.Primitive Ty.Unit)
  | Expr.Loop body ->
      let _, _, tb = infer env body in
      check (context_of env) (Ty.Primitive Ty.Unit) tb;
      literal_ty Ty.Bottom
  | Expr.While (condition, body) ->
      let _, _, tc = infer env condition in
      let _, _, tb = infer env body in
      check (context_of env) (Ty.Primitive Ty.Bool) tc;
      check (context_of env) (Ty.Primitive Ty.Unit) tb;
      literal_ty (Ty.Primitive Ty.Unit)
  | _ -> failwith "expression not yet supported"

(*( Resolve and remove any type variables: variable Tree.t -> resolved Tree.t )*)

let rec decay ?(resolve_variables = true) ?(resolve_literals = false) :
    variable Ty.t -> variable Ty.t = function
  | Ty.Integer when resolve_literals -> Ty.Primitive Ty.I32
  | Ty.Real when resolve_literals -> Ty.Primitive Ty.F32
  | Ty.Variable (Variable var) when resolve_variables -> (
      match !var with
      | Some t -> decay t
      | None -> failwith "cannot decay unresolved type variable")
  | x -> x

let rec resolve (tv : variable Ty.t) : resolved Ty.t =
  Ty.map_tv (fun (Variable var) -> resolve @@ Option.get !var)
  @@ decay ~resolve_literals:true tv

let resolve_tree ?parent m =
  ignore m;

  let solved = Tree.empty in

  { solved with parent }

(*( Apply inference and resolution to the whole module )*)

let infer_add_arguments (pos, named) =
  let env = Map.empty in

  let env =
    List.fold_left
      (fun a param ->
        match param with
        | Expr.PPValue (mode, name, ty) -> Map.create name (mode, ty) a
        | Expr.PPLabel (_name, _ty) ->
            failwith "label parameters not yet supported")
      env pos
  in

  List.fold_left
    (fun a param ->
      match param with
      | Expr.PNValue (mode, name, ty) -> Map.create name (mode, ty) a
      | Expr.PNKey (mode, name, ty, _) -> Map.create name (mode, ty) a
      | Expr.PNLabel (_name, _ty) ->
          failwith "label parameters not yet supported")
    env named

let infer_fn_definition (context : variable Tree.t) (_name : string)
    (def : variable Tree.fn_definition) =
  let return = def.s.return in

  let variables = infer_add_arguments def.s.arguments in

  let env = create_env ~return context in
  let env = add_vars env variables in

  let _, m, inferred = infer env def.value in

  (* forbid returning projected values *)
  if not Mode.(equal (unproject m) m) then
    raise @@ Mode.ProjectionFailure (Mode.unproject m, m);

  let decayed = decay ~resolve_variables:false inferred in
  check context decayed return

let infer_const_definition (context : variable Tree.t) (_name : string)
    (def : variable Tree.const_definition) =
  let env = create_env context in
  let _, _, inferred = infer env def.value in
  let decayed = decay ~resolve_variables:false inferred in
  check context decayed def.s.ty

let infer_mod (m : variable Tree.t) =
  let primitive_types =
    [
      ("bool", Ty.Primitive Bool);
      ("i8", Ty.Primitive I8);
      ("i16", Ty.Primitive I16);
      ("i32", Ty.Primitive I32);
      ("i64", Ty.Primitive I64);
      ("u8", Ty.Primitive U8);
      ("u16", Ty.Primitive U16);
      ("u32", Ty.Primitive U32);
      ("u64", Ty.Primitive U64);
      ("f32", Ty.Primitive F32);
      ("f64", Ty.Primitive F64);
    ]
  in

  let context =
    List.fold_left
      (fun (m : variable Tree.t) (k, v) ->
        let def : variable Tree.ty_definition = { ty = v } in
        let types = Map.create k def m.types in
        { m with types })
      m primitive_types
  in

  Map.iter (infer_fn_definition context) context.fn_definitions;
  Map.iter (infer_const_definition context) context.const_definitions

let f ?parent m =
  infer_mod m;

  let solved = Tree.empty in
  { solved with parent }
