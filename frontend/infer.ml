type variable = Variable of variable Ty.t option ref
type resolved = |

exception TypeMismatch of variable Ty.t * variable Ty.t

let fresh () = Variable (ref None)
let get (Variable var) = !var
let set (Variable var) x = var := Some x

(* this environment's "local context"*)
type 'tv env =
  | Root of {
      context : variable Tree.t;
      expect_return : 'tv Ty.t option;
      expect_yield : 'tv Ty.t option;
      bound : ('tv Ty.t * Mode.t) Env.t;
    }
  | Child of { super : 'tv env; bound : ('tv Ty.t * Mode.t) Env.t }

let create_env ?expect_return ?expect_yield context =
  Root { context; expect_return; expect_yield; bound = Env.empty }

let create_subenv ~super = Child { super; bound = Env.empty }

let rec lookup env binding =
  match env with
  | Root { bound; _ } -> Env.read binding bound
  | Child { bound; super; _ } -> (
      try Env.read binding bound with _ -> lookup super binding)

let rec context_of = function
  | Root { context; _ } -> context
  | Child { super; _ } -> context_of super

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

(* infer the type of an expression *)
and infer (env : variable env) (expr : variable Expr.t) : variable Ty.t =
  match expr with
  | Expr.Unit -> Ty.Primitive Ty.Unit
  | Expr.Int _ -> Ty.Integer
  | Expr.Real _ -> Ty.Real
  | Expr.Block (units, valued) -> infer_block env valued units
  | _ -> failwith "expression not yet supported"

(*( Resolve and remove any type variables: variable Tree.t -> resolved Tree.t )*)

let rec decay ?(resolve_variables = true) = function
  | Ty.Integer -> Ty.Primitive Ty.I32
  | Ty.Real -> Ty.Primitive Ty.F32
  | Ty.Variable (Variable var) when resolve_variables -> (
      match !var with
      | Some t -> decay t
      | None -> failwith "cannot decay unresolved type variable")
  | x -> x

let rec resolve (tv : variable Ty.t) : resolved Ty.t =
  Ty.map_tv (fun (Variable var) -> resolve @@ Option.get !var) @@ decay tv

let resolve_tree ?parent m =
  ignore m;

  let solved = Tree.empty in

  { solved with parent }

(*( Apply inference and resolution to the whole module )*)

let infer_fn_definition (context : variable Tree.t) (_name : string)
    (def : variable Tree.fn_definition) =
  let returns = def.s.return in

  let env = create_env ~expect_return:returns context in
  let inferred = def.value |> infer env |> decay ~resolve_variables:false in
  check ~context inferred returns

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

  Env.iter (infer_fn_definition context) context.fn_definitions

let f ?parent m =
  infer_mod m;

  let solved = Tree.empty in
  { solved with parent }
