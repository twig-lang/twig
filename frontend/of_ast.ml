let rec translate_path =
  let open Text.Ast in
  function
  | PathAtom atom -> Path.Atom atom
  | PathMember (p, c) ->
      let p = translate_path p in
      Path.Member (p, c)
  | _ -> failwith "cannot translate path"

type infer_env = {
  context : Tree.m;
  return : Tree.ty option;
  yield : Tree.ty option;
  bindings : (Tree.ty * Mode.t) Env.t;
}

let pos_param_name : Tree.positional_param -> string = function
  | Tree.Value (_, n, _) -> n
  | _ -> failwith "not implemented yet"

let pos_param_ty : Tree.positional_param -> 'a = function
  | Tree.Value (m, _, t) -> (t, m)
  | _ -> failwith "not implemented yet"

let create_infer_env ~context ?return ?yield () =
  { context; return; yield; bindings = Env.empty }

let add_paramlist env l =
  let env =
    List.fold_right
      (fun (p : Tree.positional_param) e ->
        {
          e with
          bindings = Env.create (pos_param_name p) (pos_param_ty p) e.bindings;
        })
      (fst l) env
  in

  env

exception TypeMismatch of Tree.ty * Tree.ty

let rec unify l r =
  let open Tree in
  let mismatch l r = raise @@ TypeMismatch (l, r) in
  let unify_primitives l r =
    if l <> r then mismatch (TyPrimitive l) (TyPrimitive r)
  in

  (* ty_primitive -> is_signed option *)
  let is_int_ty_primitive = function
    | T_u8 | T_u16 | T_u32 | T_u64 -> Some false
    | T_i8 | T_i16 | T_i32 | T_i64 -> Some true
    | _ -> None
  in

  (* ty_primitive -> is_f32 option *)
  let is_real_ty_primitive = function
    | T_f32 -> Some true
    | T_f64 -> Some false
    | _ -> None
  in

  match (l, r) with
  | TyPrimitive a, TyPrimitive b ->
      unify_primitives a b;
      l
  | TyInteger, TyPrimitive p when Option.is_some @@ is_int_ty_primitive p -> r
  | TyPrimitive p, TyInteger when Option.is_some @@ is_int_ty_primitive p -> r
  | TyReal, TyPrimitive p when Option.is_some @@ is_real_ty_primitive p -> r
  | TyPrimitive p, TyReal when Option.is_some @@ is_real_ty_primitive p -> r
  | TyBottom, _ -> r
  | _, TyBottom -> l
  | TyUnknown l, _ -> (
      match !l with
      | None ->
          l := Some r;
          r
      | Some t -> unify t r)
  | _, TyUnknown l -> (
      match !l with
      | None ->
          l := Some r;
          r
      | Some t -> unify t r)
  | TyPointer l, TyPointer r -> TyPointer (unify l r)
  | TySlice l, TySlice r -> TySlice (unify l r)
  | TyNamed nl, TyNamed nr -> if Path.equal nl nr then l else mismatch l r
  | TyTuple tl, TyTuple tr -> TyTuple (List.map2 unify tl tr)
  | _ -> mismatch l r

let translate_mode (Text.Ast.Mode (r, m)) = Mode.create ~mut:m ~reference:r ()

let rec must_args (env : infer_env) (s : Tree.fn_signature) p n =
  let open Text.Ast in
  let must_pos_arg (spos : Tree.positional_param) (pos : argument) (env, a) =
    match (spos, pos) with
    | Tree.Value (mp, _, tv), Argument (ma, e) ->
        let ma = translate_mode ma in

        if not @@ Mode.(mp >: ma) then failwith "incompatible modes";

        let te, v, env = infer env e in
        ignore @@ unify tv te;

        (env, Tree.Value (mp, v) :: a)
    | _ -> failwith "argument mismatch"
  in
  let must_named_arg (snam : Tree.named_param) (nam : named_argument) (_env, _a)
      =
    match (snam, nam) with _ -> failwith "argument mismatch"
  in

  let env, positional =
    List.fold_right2 must_pos_arg (fst s.arguments) p (env, [])
  in
  let env, named =
    List.fold_right2 must_named_arg (snd s.arguments) n (env, [])
  in

  (env, List.rev positional, List.rev named)

and infer (env : infer_env) =
  let open Text.Ast in
  let open Tree in
  let rec infer_block env a = function
    | [] -> (env, TyPrimitive T_unit, a, EUnit)
    | t :: [] ->
        let tt, t, env = infer env t in
        (env, tt, List.rev a, t)
    | t :: ts ->
        let tt, t, env = infer env t in
        let _ = unify tt (TyPrimitive T_unit) in
        infer_block env (t :: a) ts
  in

  function
  | ExprUnit -> (TyPrimitive T_unit, EUnit, env)
  | ExprBool b -> (TyPrimitive T_bool, EBool b, env)
  | ExprInteger n -> (TyInteger, EInt n, env)
  | ExprReal f -> (TyReal, EReal f, env)
  | ExprIf (c, t, f) ->
      let tc, c, env = infer env c in
      let tt, t, env = infer env t in
      let tf, f, env = infer env f in

      let _ = unify tc (TyPrimitive T_bool) in
      let ti = unify tt tf in

      (ti, EIf (c, t, f), env)
  | ExprWhen (c, b) ->
      let tc, c, env = infer env c in
      let tb, b, env = infer env b in

      let _ = unify tc (TyPrimitive T_bool) in
      let _ = unify tb (TyPrimitive T_unit) in

      (tb, EWhen (c, b), env)
  | ExprReturn None ->
      ignore @@ unify (TyPrimitive T_unit) @@ Option.get env.return;

      (TyBottom, EReturn EUnit, env)
  | ExprReturn (Some v) ->
      let ty, v, env = infer env v in

      ignore @@ unify ty @@ Option.get env.return;

      (TyBottom, EReturn v, env)
  | ExprBlock items ->
      let env, returned, units, value = infer_block env [] items in
      (returned, EBlock (units, value), env)
  | ExprLet (pat, _ty, _mode, value) ->
      let env =
        match pat with
        | PatNamed (PathAtom name) ->
            let t, _, env = infer env value in
            let binds = Env.create name (t, Mode.create ()) env.bindings in
            { env with bindings = binds }
        | _ -> failwith "unsupported pattern"
      in
      (TyPrimitive T_unit, EUnit, env)
  | ExprVariable path -> (
      match path with
      | PathAtom atom ->
          let ty, _ = Env.read atom env.bindings in
          (ty, Tree.EVariable (translate_path path), env)
      | _ -> failwith "unsupported path")
  | ExprCall (fn, positional, named) ->
      let name =
        match fn with
        | ExprVariable path -> translate_path path
        | _ -> failwith "unsupported callee"
      in

      let s = Tree.get_fnsig name env.context in

      let env, positional, named = must_args env s positional named in

      (s.return, Tree.ECall (Tree.EVariable name, positional, named), env)
  | _ -> failwith "unsupported AST node"

let translate_type env =
  let open Text.Ast in
  let open Tree in
  function
  | TyUnit -> TyPrimitive T_unit
  | TyNamed name ->
      let path = translate_path name in
      let def = get_ty path env in
      def.ty
  | _ -> failwith "cannot translate type"

let translate_param_list env p n : Tree.param_list =
  let open Text.Ast in
  let translate_pos_param env : parameter -> Tree.positional_param = function
    | Parameter (Mode (r, m), name, ty) ->
        let mode = Mode.create ~mut:m ~reference:r () in
        let ty = translate_type env ty in
        Tree.Value (mode, name, ty)
    | ParameterLabel (name, ty) ->
        let ty = translate_type env ty in
        Tree.Label (name, ty)
    | ParameterKey _ -> failwith "named argument in positional argument list"
  in
  let rec translate_named_param env = function
    | Parameter (Mode (r, m), name, ty) ->
        let mode = Mode.create ~mut:m ~reference:r () in
        let ty = translate_type env ty in
        (Tree.Value (mode, name, ty) : Tree.named_param)
    | ParameterLabel (name, ty) ->
        let ty = translate_type env ty in
        Tree.Label (name, ty)
    | ParameterKey (Mode (r, m), name, default, ty') -> (
        let mode = Mode.create ~mut:m ~reference:r () in
        let ty = translate_type env ty' in

        match default with
        | Some e ->
            let ienv : infer_env = create_infer_env ~context:env () in

            let vt, v, _ = infer ienv e in
            ignore @@ unify ty vt;

            Tree.Key (mode, name, ty, v)
        | None -> translate_named_param env (Parameter (Mode (r, m), name, ty'))
        )
  in

  let positional = List.map (translate_pos_param env) p in
  let named = List.map (translate_named_param env) n in
  (positional, named)

let of_toplevel top env =
  let open Text.Ast in
  let open Tree in
  match top with
  | TopFnDefinition { name; ty; value; pos_parameters; key_parameters; _ } -> (
      let return =
        match ty with
        | None -> TyPrimitive T_unit
        | Some t -> translate_type env t
      in

      let arguments = translate_param_list env pos_parameters key_parameters in

      let ienv : infer_env = create_infer_env ~context:env ~return () in

      let ienv = add_paramlist ienv arguments in

      let s : Tree.fn_signature = { return; arguments } in

      let env =
        { env with fn_signatures = Env.create name s env.fn_signatures }
      in

      match value with
      | Some value ->
          let value_type, value, _ = infer ienv value in
          ignore @@ unify value_type return;

          let def : Tree.fn_definition = { s; value } in

          { env with fn_definitions = Env.create name def env.fn_definitions }
      | None -> env)
  | TopConstDefinition { name; ty; value } ->
      let ty = translate_type env ty in

      let ienv : infer_env = create_infer_env ~context:env () in

      let value_type, value, _ = infer ienv value in

      ignore @@ unify ty value_type;

      let s = { ty } in
      let def = { s; value } in

      {
        env with
        const_definitions = Env.create name def env.const_definitions;
        const_signatures = Env.create name s env.const_signatures;
      }
  | _ -> failwith "unsupported as of now!"

let default_toplevel () =
  let open Tree in
  let env = Tree.empty in

  let types =
    [
      ("bool", TyPrimitive T_bool);
      ("i8", TyPrimitive T_i8);
      ("i16", TyPrimitive T_i16);
      ("i32", TyPrimitive T_i32);
      ("i64", TyPrimitive T_i64);
      ("u8", TyPrimitive T_u8);
      ("u16", TyPrimitive T_u16);
      ("u32", TyPrimitive T_u32);
      ("u64", TyPrimitive T_u64);
      ("f32", TyPrimitive T_f32);
      ("f64", TyPrimitive T_f64);
    ]
  in

  let env =
    List.fold_right
      (fun (n, ty) e ->
        let def : ty_definition = { ty } in
        { e with types = Env.create n def e.types })
      types env
  in

  env

let of_ast ast =
  let env = default_toplevel () in
  List.fold_left (fun t e -> of_toplevel e t) env ast
