type variable = Variable of variable Ty.t option ref
type resolved = |

exception TypeMismatch of variable Ty.t * variable Ty.t

let fresh () = Variable (ref None)
let get (Variable var) = !var
let set (Variable var) x = var := Some x

type 'tv expect = { return : 'tv Ty.t option; yield : 'tv Ty.t option }

(* this environment's "local context"*)
type 'tv env =
  | Root of {
      context : variable Tree.t;
      expect : 'tv expect;
      variables : ('tv Ty.t * Mode.t) Env.t;
    }
  | Child of { super : 'tv env; bound : ('tv Ty.t * Mode.t) Env.t }

let create_env ?return ?yield ?(variables = Env.empty) context =
  let expect = { return; yield } in
  Root { context; expect; variables }

let create_subenv ~super = Child { super; bound = Env.empty }

let rec lookup_variable env binding =
  match env with
  | Root { variables; _ } -> Env.read_opt binding variables
  | Child { bound; super; _ } -> (
      try Some (Env.read binding bound)
      with _ -> lookup_variable super binding)

let rec context_of = function
  | Root { context; _ } -> context
  | Child { super; _ } -> context_of super

let rec expect_of = function
  | Root { expect; _ } -> expect
  | Child { super; _ } -> expect_of super

(* Create a Tree from a list of toplevel statements *)
let tree_of_toplevels tops = List.fold_left Tree.add Tree.empty tops

(*( Perform type-checking and inference )*)
let mismatch l r = raise @@ TypeMismatch (l, r)

let unify_primitive l r =
  if Ty.equal_prim l r then Ty.Primitive l
  else mismatch (Ty.Primitive l) (Ty.Primitive r)

let resolve_named ~context = function
  | Ty.Named name ->
      let def = Tree.get_ty name context in
      def.ty
  | x -> x

(* "unify" two types, and return the unified version *)
let rec unify ~context (l : variable Ty.t) (r : variable Ty.t) =
  let l = resolve_named ~context l in
  let r = resolve_named ~context r in

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
      | Some t -> unify ~context t r
      | None ->
          set l r;
          r)
  | _, Ty.Variable r -> (
      match get r with
      | Some t -> unify ~context l t
      | None ->
          set r l;
          l)
  (* TODO: The rest of the cases. *)
  (* Any other case is a mismatch *)
  | _ -> mismatch l r

(* only perform the type check *)
let check ~context l r = ignore @@ unify ~context l r

let rec infer_block (env : variable env) valued = function
  | x :: xs ->
      let tx = infer env x in
      check ~context:(context_of env) (Ty.Primitive Ty.Unit) tx;

      infer_block env valued xs
  | [] -> infer env valued

and check_arguments env (p, n) positional named =
  (* TODO: handle labels later on *)
  (* TODO: at least type check this *)
  List.iter2
    (fun param (Expr.AValue (am, av)) ->
      match param with
      | Expr.PPValue (m, _, t) ->
          let at = infer env av in
          ignore @@ Mode.project m am;
          check ~context:(context_of env) t at
      | Expr.PPLabel _ -> failwith "label parameters not supported yet")
    p positional;

  (* TODO: named arguments are not checked in order,
    maybe turn them from a list into an Env.t? *)
  List.iter2 (fun _param _arg -> ()) n named;

  ()

(* infer the type of an expression *)
and infer (env : variable env) (expr : variable Expr.t) : variable Ty.t =
  match expr with
  | Expr.Unit -> Ty.Primitive Ty.Unit
  | Expr.Int _ -> Ty.Integer
  | Expr.Real _ -> Ty.Real
  | Expr.Block (units, valued) -> infer_block env valued units
  | Expr.Call (Expr.Variable name, positional, named) ->
      (* TODO: support actual callable values *)
      let fn = Tree.get_fnsig name (context_of env) in
      check_arguments env fn.arguments positional named;
      fn.return
  | Expr.Variable (Path.Atom name) -> (
      match lookup_variable env name with
      | Some (ty, _) -> ty
      | None ->
          let s = Tree.get_ksig (Path.Atom name) (context_of env) in
          s.ty)
  | Expr.Return value -> (
      let exp = expect_of env in
      let tv = infer env value in
      match exp.return with
      | Some ty ->
          check ~context:(context_of env) ty tv;
          Ty.Bottom
      | None -> failwith "unexpected return expression")
  | Expr.If (condition, t, f) ->
      let tc = infer env condition in
      let tt = infer env t in
      let tf = infer env f in
      check ~context:(context_of env) (Ty.Primitive Ty.Bool) tc;
      unify ~context:(context_of env) tt tf
  | _ -> failwith "expression not yet supported"

(*( Resolve and remove any type variables: variable Tree.t -> resolved Tree.t )*)

let rec decay ?(resolve_variables = true) ?(resolve_literals = false) = function
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
  let env = Env.empty in

  let env =
    List.fold_left
      (fun a param ->
        match param with
        | Expr.PPValue (mode, name, ty) -> Env.create name (ty, mode) a
        | Expr.PPLabel (_name, _ty) ->
            failwith "label parameters not yet supported")
      env pos
  in

  List.fold_left
    (fun a param ->
      match param with
      | Expr.PNValue (mode, name, ty) -> Env.create name (ty, mode) a
      | Expr.PNKey (mode, name, ty, _) -> Env.create name (ty, mode) a
      | Expr.PNLabel (_name, _ty) ->
          failwith "label parameters not yet supported")
    env named

let infer_fn_definition (context : variable Tree.t) (_name : string)
    (def : variable Tree.fn_definition) =
  let return = def.s.return in

  let variables = infer_add_arguments def.s.arguments in

  let env = create_env ~return ~variables context in

  let inferred = def.value |> infer env |> decay ~resolve_variables:false in
  check ~context inferred return

let infer_const_definition (context : variable Tree.t) (_name : string)
    (def : variable Tree.const_definition) =
  let env = create_env context in
  let inferred = def.value |> infer env |> decay ~resolve_variables:false in
  check ~context inferred def.s.ty

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
        let types = Env.create k def m.types in
        { m with types })
      m primitive_types
  in

  Env.iter (infer_fn_definition context) context.fn_definitions;
  Env.iter (infer_const_definition context) context.const_definitions

let f ?parent m =
  infer_mod m;

  let solved = Tree.empty in
  { solved with parent }
