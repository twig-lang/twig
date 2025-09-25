module Stage :
  Tree.STAGE with type ty_variable_proof = unit with type variable_name = Path.t =
struct
  type ty_variable_proof = unit
  type variable_name = Path.t

  let fmt_variable_name = Path.fmt
end

module T = Tree.TreeS (Stage)

type infer_env = {
  context : T.m;
  return : T.ty option;
  yield : T.ty option;
  bindings : (T.ty * Mode.t * T.expr) Env.t;
}

let create_infer_env ~context ?return ?yield () =
  { context; return; yield; bindings = Env.empty }

exception TypeMismatch of T.ty * T.ty

let rec unify l r =
  let open T in
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
  | TyUnknown (_, l), _ -> (
      match !l with
      | None ->
          l := Some r;
          r
      | Some t -> unify t r)
  | _, TyUnknown (_, l) -> (
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

let rec infer (env : infer_env) =
  let open Text.Ast in
  let open T in
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

      (ti, EIf (ti, c, t, f), env)
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
      (returned, EBlock (returned, units, value), env)
  | ExprLet (pat, _ty, _mode, value) ->
      let env =
        match pat with
        | PatNamed (PathAtom name) ->
            let t, e, env = infer env value in
            let binds = Env.create name (t, Mode.create (), e) env.bindings in
            { env with bindings = binds }
        | _ -> failwith "unsupported pattern"
      in
      (TyPrimitive T_unit, EUnit, env)
  | ExprVariable path -> (
      match path with
      | PathAtom atom ->
          let ty, _, value = Env.read atom env.bindings in
          (ty, value, env)
      | _ -> failwith "unsupported path")
  | _ -> failwith "unsupported AST node"

let rec translate_path =
  let open Text.Ast in
  function
  | PathAtom atom -> Path.Atom atom
  | PathMember (p, c) ->
      let p = translate_path p in
      Path.Member (p, c)
  | _ -> failwith "cannot translate path"

let translate_type env =
  let open Text.Ast in
  let open T in
  function
  | TyUnit -> TyPrimitive T_unit
  | TyNamed name ->
      let path = translate_path name in
      let def = get_ty path env in
      def.ty
  | _ -> failwith "cannot translate type"

let of_toplevel top env =
  let open Text.Ast in
  let open T in
  match top with
  | TopFnDefinition { name; ty; value; _ } -> (
      let name =
        match name with FnNamed plain -> plain | FnOperator op -> op
      in

      let return =
        match ty with
        | None -> TyPrimitive T_unit
        | Some t -> translate_type env t
      in

      let ienv : infer_env = create_infer_env ~context:env ~return () in

      match value with
      | Some value ->
          let value_type, value, _ = infer ienv value in
          ignore @@ unify value_type return;

          let def : T.fn_definition = { return; value } in
          let s : T.fn_signature = { return } in

          {
            env with
            fn_definitions = Env.create name def env.fn_definitions;
            fn_signatures = Env.create name s env.fn_signatures;
          }
      | None ->
          let s : T.fn_signature = { return } in
          { env with fn_signatures = Env.create name s env.fn_signatures })
  | TopConstDefinition { name; ty; value } ->
      let ty = translate_type env ty in

      let ienv : infer_env = create_infer_env ~context:env () in

      let value_type, value, _ = infer ienv value in

      ignore @@ unify ty value_type;

      let def = { ty; value } in
      let s = { ty } in

      {
        env with
        const_definitions = Env.create name def env.const_definitions;
        const_signatures = Env.create name s env.const_signatures;
      }
  | _ -> failwith "unsupported as of now!"

let default_toplevel () =
  let open T in
  let env = T.empty in

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
  List.fold_right of_toplevel ast env
