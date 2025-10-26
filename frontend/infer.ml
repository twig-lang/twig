type ty = Env.variable Ty.t

exception TypeMismatch of ty * ty

let fresh () = Env.Variable (ref None)
let get (Env.Variable var) = !var
let set (Env.Variable var) x = var := Some x

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
let rec unify context (l : ty) (r : ty) =
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
      check (Env.context env) (Ty.Primitive Ty.Unit) tx;

      infer_block env valued xs
  | [] -> infer env valued

and check_arguments env (p, _n) positional named =
  (* TODO: handle labels later on *)
  (* TODO: at least type check this *)
  List.iter2
    (fun param (Expr.AValue (am, av)) ->
      match param with
      | Expr.PPValue (m, _, t) ->
          let env, _, at = infer env av in
          ignore @@ Mode.project m am;
          check (Env.context env) t at
      | Expr.PPLabel _ -> failwith "label parameters not supported yet")
    p positional;

  (* TODO: named arguments are not checked in order,
    maybe turn them from a list into an Env.t? *)
  List.iter (fun _arg -> ()) named;

  ()

(* infer the type of an expression *)
and infer (env : Env.t) expr : Env.t * Mode.t * ty =
  let literal_ty ty = (env, Mode.create (), ty) in
  let literal_ty' ty = (env, Mode.create ~mut:Mode.Immutable (), ty) in

  match expr with
  | Expr.Unit -> literal_ty (Ty.Primitive Ty.Unit)
  | Expr.Int _ -> literal_ty Ty.Integer
  | Expr.Real _ -> literal_ty Ty.Real
  | Expr.Bool _ -> literal_ty (Ty.Primitive Ty.Bool)
  | Expr.Char _ -> literal_ty (Ty.Primitive Ty.Char)
  | Expr.String _ -> literal_ty' (Ty.Primitive Ty.Str)
  | Expr.Block (units, valued) -> infer_block env valued units
  | Expr.FnCall (Expr.Variable name, positional, named) ->
      (* TODO: support actual callable values *)
      let fn = Tree.get_fnsig name (Env.context env) in
      check_arguments env fn.arguments positional named;
      literal_ty fn.return
  | Expr.FnCall _ -> failwith "unsupported callee"
  | Expr.Variable (Path.Atom name) -> (
      match Env.find_variable env name with
      | Some (m, ty) -> (env, m, ty)
      | None ->
          let s = Tree.get_ksig (Path.Atom name) (Env.context env) in
          literal_ty s.ty)
  | Expr.Variable _ -> failwith "unsupported variable path"
  | Expr.Return value -> (
      let exp = Env.expect env in
      let env, m, tv = infer env value in

      (* forbid returning projected values *)
      if not Mode.(equal (unproject m) m) then
        raise @@ Mode.ProjectionFailure (Mode.unproject m, m);

      match exp.return with
      | Some ty ->
          check (Env.context env) ty tv;
          literal_ty Ty.Bottom
      | None -> failwith "unexpected return expression")
  | Expr.If (condition, t, f) ->
      let env, _, tc = infer env condition in
      let env, _, tt = infer env t in
      let env, _, tf = infer env f in
      check (Env.context env) (Ty.Primitive Ty.Bool) tc;
      literal_ty @@ unify (Env.context env) tt tf
  | Expr.Let (name, mode, ty, value) ->
      let ty = Option.value ~default:(Ty.Variable (fresh ())) ty in
      let _, m, tv = infer env value in
      ignore @@ Mode.project mode m;
      check (Env.context env) ty tv;
      let env = Env.add_var env name mode tv in
      (env, Mode.create (), Ty.Primitive Ty.Unit)
  | Expr.Loop body ->
      let _, _, tb = infer env body in
      check (Env.context env) (Ty.Primitive Ty.Unit) tb;
      literal_ty Ty.Bottom
  | Expr.While (condition, body) ->
      let _, _, tc = infer env condition in
      let _, _, tb = infer env body in
      check (Env.context env) (Ty.Primitive Ty.Bool) tc;
      check (Env.context env) (Ty.Primitive Ty.Unit) tb;
      literal_ty (Ty.Primitive Ty.Unit)
  | Expr.Label (name, ty, body) ->
      let env = Env.add_label env name ty in
      let _, _, tb = infer env body in
      check (Env.context env) ty tb;
      (env, Mode.create (), ty)
  | Expr.Break (name, value) ->
      let _, _, tv = infer env value in

      let t_label = Env.find_label env name in
      (* TODO: maybe an exception here? *)
      let t_label = Option.get t_label in

      check (Env.context env) tv t_label;

      literal_ty Ty.Bottom
  | Expr.List _ | Expr.Tuple _ -> failwith "expression not yet supported"

(*( Resolve and remove any type variables: variable Tree.t -> resolved Tree.t )*)

let rec decay ?(resolve_variables = true) ?(resolve_literals = false) : ty -> ty
    = function
  | Ty.Integer when resolve_literals -> Ty.Primitive Ty.I32
  | Ty.Real when resolve_literals -> Ty.Primitive Ty.F32
  | Ty.Variable (Variable var) when resolve_variables -> (
      match !var with
      | Some t -> decay t
      | None -> failwith "cannot decay unresolved type variable")
  | x -> x

let rec resolve (tv : ty) : Env.resolved Ty.t =
  Ty.map_tv (fun (Env.Variable var) -> resolve @@ Option.get !var)
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
    (fun a (name, param) ->
      match param with
      | Expr.PNValue (mode, ty) -> Map.create name (mode, ty) a
      | Expr.PNKey (mode, ty, _) -> Map.create name (mode, ty) a
      | Expr.PNLabel _ty -> failwith "label parameters not yet supported")
    env (Map.to_list named)

let infer_fn_definition (context : Env.variable Tree.t) (_name : string)
    (def : Env.variable Tree.fn_definition) =
  let return = def.s.return in

  let variables = infer_add_arguments def.s.arguments in

  let env = Env.create ~return context in
  let env = Env.add_vars env variables in

  let _, m, inferred = infer env def.value in

  (* forbid returning projected values *)
  if not Mode.(equal (unproject m) m) then
    raise @@ Mode.ProjectionFailure (Mode.unproject m, m);

  let decayed = decay ~resolve_variables:false inferred in
  check context decayed return

let infer_const_definition (context : Env.variable Tree.t) (_name : string)
    (def : Env.variable Tree.const_definition) =
  let env = Env.create context in
  let _, _, inferred = infer env def.value in
  let decayed = decay ~resolve_variables:false inferred in
  check context decayed def.s.ty

let infer_mod (m : Env.variable Tree.t) =
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
      ("char", Ty.Primitive Char);
      ("str", Ty.Primitive Str);
    ]
  in

  let context =
    List.fold_left
      (fun (m : Env.variable Tree.t) (k, v) ->
        let def : Env.variable Tree.ty_definition = { ty = v } in
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
